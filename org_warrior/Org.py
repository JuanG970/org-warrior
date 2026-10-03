from dataclasses import dataclass, field
from typing import Optional, Tuple
from datetime import datetime, date
import logging
import os
import re
import json


def _escape_elisp(s: str) -> str:
    """Escape string for use in elisp code."""
    return s.replace("\\", "\\\\").replace('"', '\\"')


# Date parsing utilities
ORG_TS_RE = re.compile(r"(\d{4}-\d{2}-\d{2})(?:\s+\w+)?(?:\s+(\d{2}:\d{2}))?")


def parse_org_timestamp(value: Optional[str]) -> Tuple[Optional[date], Optional[str]]:
    """Parse org timestamp string and return (date, time)."""
    if not value:
        return None, None
    # Handle if it's already a datetime object
    if isinstance(value, datetime):
        return value.date(), value.strftime("%H:%M")
    if isinstance(value, date):
        return value, None
    match = ORG_TS_RE.search(str(value))
    if not match:
        return None, None
    date_str, time_str = match.groups()
    try:
        parsed_date = datetime.strptime(date_str, "%Y-%m-%d").date()
    except ValueError:
        return None, time_str
    return parsed_date, time_str


def date_status(value: Optional[date]) -> Optional[str]:
    """Return status of date: overdue, today, or future."""
    if not value:
        return None
    today = date.today()
    if value < today:
        return "overdue"
    if value == today:
        return "today"
    return "future"


def resolve_org_files(files_spec) -> list[str]:
    """Return a list of .org files from a file/dir or path-separated list, including nested."""
    if isinstance(files_spec, (list, tuple)):
        specs = files_spec
    else:
        specs = str(files_spec).split(os.pathsep)
    resolved = []
    for spec in specs:
        if not spec:
            continue
        path = os.path.expanduser(spec)
        if os.path.isdir(path):
            for root, _, filenames in os.walk(path):
                for fname in filenames:
                    if fname.endswith(".org"):
                        resolved.append(os.path.join(root, fname))
        elif os.path.isfile(path):
            resolved.append(path)
    # Deduplicate preserving first-seen order
    seen = set()
    unique = []
    for f in resolved:
        if f not in seen:
            seen.add(f)
            unique.append(f)
    return unique


def _tokenize_sexp(s: str) -> list:
    r"""Tokenize a flat s-expression into a list of Python values.

    Handles escaped characters inside strings (\" → ", \\ → \).
    Returns strings, integers, and None (for nil).
    """
    tokens = []
    i = 0
    while i < len(s):
        if s[i] in " \t":
            i += 1
        elif s[i] == '"':
            # Parse quoted string with escape handling
            i += 1  # skip opening quote
            chars = []
            while i < len(s) and s[i] != '"':
                if s[i] == "\\" and i + 1 < len(s):
                    chars.append(s[i + 1])
                    i += 2
                else:
                    chars.append(s[i])
                    i += 1
            i += 1  # skip closing quote
            tokens.append("".join(chars))
        elif s[i : i + 3] == "nil":
            tokens.append(None)
            i += 3
        elif s[i].isdigit() or (s[i] == "-" and i + 1 < len(s) and s[i + 1].isdigit()):
            j = i + 1 if s[i] != "-" else i + 2
            while j < len(s) and s[j].isdigit():
                j += 1
            tokens.append(int(s[i:j]))
            i = j
        else:
            i += 1  # skip parens and other delimiters
    return tokens


def parse_org_ql_result(result: Optional[str]) -> list | dict:
    """Parse a single result line from org-ql output. Should be JSON output since org-warrior version 1.1.0"""
    # Handle None or empty string from connection errors/timeouts
    if result is None or result == "":
        return []

    try:
        # Check if the string contains ERROR: <ERROR MESSAGE> if so return the failure
        if result.startswith("ERROR:"):
            error_message = result[6:].strip()
            logging.error(f"Error from org-ql: {error_message}")
            raise RuntimeError(f"Error from org-ql: {error_message}")
        parsed = json.loads(result)
        if parsed is None:
            return []
        return parsed
    except json.JSONDecodeError as e:
        logging.error(f"Error parsing org-ql result: {e}")
        raise


class OrgQL:
    """
    org-ql query builder and executor.
    """

    @staticmethod
    def _parse_filter(arg: str) -> Optional[str]:
        """Parse a single filter token into an org-ql condition, or None if skipped."""
        if arg.startswith("+"):
            return f'(tags "{_escape_elisp(arg[1:])}")'
        elif arg.startswith("project:"):
            return '(ancestors (tags "project"))'
        elif arg.startswith("pri:") or arg.startswith("priority:"):
            pri = arg.split(":")[1].upper()
            return f'(priority "{_escape_elisp(pri)}")'
        elif arg.startswith("due:"):
            due = arg.split(":")[1]
            if due == "today":
                return "(deadline :on today)"
            elif due == "week":
                return "(deadline :from today :to +7)"
            elif due == "overdue":
                return "(deadline :to -1)"
        elif arg.startswith("state:") or arg.startswith("status:"):
            state = arg.split(":")[1].upper()
            return f'(todo "{_escape_elisp(state)}")'
        elif arg.startswith("scheduled:"):
            sched = arg.split(":")[1]
            if sched == "today":
                return "(scheduled :on today)"
        else:
            return f'(regexp "{_escape_elisp(arg)}")'
        return None

    @staticmethod
    def _wrap_conditions(conditions: list[str]) -> str:
        """Wrap a list of conditions: single = bare, multiple = (and ...)."""
        conditions = [c for c in conditions if c]
        if not conditions:
            return "(and (todo) (not (done)))"
        if len(conditions) == 1:
            return conditions[0]
        return f"(and {' '.join(conditions)})"

    @staticmethod
    def build_query(query: str) -> str:
        """
        Build org-ql query from TaskWarrior-like filters.

        Supports boolean operators:
        - Tokens are implicitly AND-ed within a group.
        - ``or`` splits into groups combined with ``(or ...)``.
        - ``and`` is accepted for readability but is the default.

        Examples:
            state:AGENT or state:STRT
            +work state:TODO
            state:CODE or state:REVIEW +urgent
        """
        if query.strip() == "":
            return "(and (todo) (not (done)))"

        args = query.split()
        logging.debug(f"Parsing query args: {args}")

        # Split args into groups separated by "or"
        groups: list[list[str]] = [[]]
        for arg in args:
            lower = arg.lower()
            if lower == "or":
                groups.append([])
            elif lower == "and":
                continue  # explicit AND is a no-op (default behaviour)
            else:
                groups[-1].append(arg)

        # Parse each group into conditions
        or_branches = []
        for group in groups:
            conditions = []
            for arg in group:
                cond = OrgQL._parse_filter(arg)
                if cond:
                    conditions.append(cond)
            if conditions:
                or_branches.append(OrgQL._wrap_conditions(conditions))

        if not or_branches:
            return "(and (todo) (not (done)))"
        if len(or_branches) == 1:
            return or_branches[0]
        return f"(or {' '.join(or_branches)})"

    @staticmethod
    def get_task_by_id(org_id: str) -> Optional["Task"]:
        """Get a single task by its Org ID."""
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "get-task-by-id.el", params={"org_id": org_id, "files": files_quoted}
            )
            if not result or result.strip() == '""':
                return None

            parsed = parse_org_ql_result(result)
            # Handle case where parse_org_ql_result returns empty list (connection error)
            if not isinstance(parsed, dict):
                return None
            if parsed.get("id"):
                # Get the task properties
                return Task(
                    org_id=parsed["id"],
                    title=parsed["heading"],
                    tags=parsed.get("tags", []),
                    location=parsed.get("filename", "")
                    + ":"
                    + str(parsed.get("linenumber", "")),
                    status=parsed.get("todo"),
                    priority=parsed.get("priority"),
                    deadline=parsed.get("deadline"),
                    scheduled=parsed.get("scheduled"),
                    properties=parsed.get("properties", {}),
                )
            return None
        except Exception as e:
            logging.error(f"Error getting task by ID: {e}")
            return None

    @staticmethod
    def get_task_body(org_id: str) -> Optional[str]:
        """Get the body content of a task by its Org ID."""
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        try:
            result = emacs_run_elisp_file("get-task-body.el", params={"org_id": org_id})

            if not result:
                return None

            # Remove outer quotes if present
            if result.startswith('"') and result.endswith('"'):
                result = result[1:-1]

            if result == "NOT_FOUND":
                return None

            # Unescape elisp string
            return (
                result.replace("\\n", "\n")
                .replace("\\t", "\t")
                .replace('\\"', '"')
                .replace("\\\\", "\\")
            )
        except Exception as e:
            logging.error(f"Error getting task body: {e}")
            return None

    @staticmethod
    def run_query(
        query_str: str, files=None, assign_handles: bool = True
    ) -> list["Task"]:
        """Run org-ql query and return list of Task objects."""
        from org_warrior import config
        from org_warrior.HandleCache import HandleCache
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = files or config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        logging.debug(f"Running org-ql query: {query_str}")
        logging.debug(f"On files: {files_quoted}")

        # Execute org-ql query using elisp template
        try:
            result = emacs_run_elisp_file(
                "org-ql-select.el", params={"files": files_quoted, "query": query_str}
            )

            handle_cache = HandleCache() if assign_handles else None
            parsed = parse_org_ql_result(result)
            tasks = []
            for item in parsed:
                # Skip tasks without ID property
                if item.get("id") is None:
                    logging.warning(
                        f"Skipping task without ID: '{item.get('heading', 'Unknown')}' "
                        f"at {item.get('filename', 'Unknown')}:{item.get('linenumber', '?')}"
                    )
                    continue
                
                task = Task(
                    org_id=item["id"],
                    title=item["heading"],
                    tags=item.get("tags", []),
                    location=item.get("filename", "")
                    + ":"
                    + str(item.get("linenumber", "")),
                    status=item.get("todo"),
                    priority=item.get("priority"),
                    deadline=item.get("deadline"),
                    scheduled=item.get("scheduled"),
                    handle=handle_cache.get_handle(item["id"])
                    if handle_cache
                    else None,
                    properties=item.get("properties", {}),
                )
                tasks.append(task)

            # Save handle cache if modified
            if handle_cache:
                handle_cache.save()

            return tasks
        except Exception as e:
            logging.error(f"Error running org-ql: {e}")
            return []

    @staticmethod
    def add_task(
        title: str, inbox_file: str = "~/org/inbox.org", inbox_heading: str = "Inbox"
    ) -> Optional[str]:
        """
        Add a new TODO task to the inbox.

        Args:
            title: Task title
            inbox_file: Path to inbox file (default: ~/org/inbox.org)
            inbox_heading: Heading under which to add task (default: Inbox)

        Returns:
            The Org ID of the created task, or None on error
        """
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        try:
            result = emacs_run_elisp_file(
                "add-task.el",
                params={
                    "title": title,
                    "file": inbox_file,
                    "heading": inbox_heading,
                },
            )
            if result:
                return result.strip()
            return None
        except Exception as e:
            logging.error(f"Error adding task: {e}")
            return None

    @staticmethod
    def clock_in(org_id: str) -> tuple[bool, str]:
        """
        Clock in to a task by Org ID.

        Args:
            org_id: The Org ID of the task

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "clock-in.el", params={"org_id": org_id, "files": files_quoted}
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error clocking in: {e}")
            return False, str(e)

    @staticmethod
    def clock_out() -> tuple[bool, str]:
        """
        Clock out from the current task.

        Returns:
            Tuple of (success, message)
        """
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        try:
            result = emacs_run_elisp_file("clock-out.el", params={})
            if not result:
                return False, "No response from Emacs"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, "Clocked out successfully"
        except Exception as e:
            logging.error(f"Error clocking out: {e}")
            return False, str(e)

    @staticmethod
    def schedule(org_id: str, date: str) -> tuple[bool, str]:
        """
        Schedule a task for a specific date.

        Args:
            org_id: The Org ID of the task
            date: Date string (e.g., "today", "+1d", "2024-12-25")

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "schedule.el",
                params={"org_id": org_id, "date": date, "files": files_quoted},
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error scheduling: {e}")
            return False, str(e)

    @staticmethod
    def deadline(org_id: str, date: str) -> tuple[bool, str]:
        """
        Set a deadline for a task.

        Args:
            org_id: The Org ID of the task
            date: Date string (e.g., "today", "+1d", "2024-12-25")

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "deadline.el",
                params={"org_id": org_id, "date": date, "files": files_quoted},
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error setting deadline: {e}")
            return False, str(e)

    @staticmethod
    def set_state(org_id: str, state: str) -> tuple[bool, str]:
        """
        Set the TODO state of a task.

        Args:
            org_id: The Org ID of the task
            state: TODO state (e.g., "TODO", "DONE", "STRT")

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "set-state.el",
                params={"org_id": org_id, "state": state, "files": files_quoted},
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error setting state: {e}")
            return False, str(e)

    @staticmethod
    def modify_tags(org_id: str, action: str, tag: str) -> tuple[bool, str]:
        """
        Add or remove a tag on a task.

        Args:
            org_id: The Org ID of the task
            action: "add" or "remove"
            tag: Tag name (without +/- prefix)

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "modify-tags.el",
                params={
                    "org_id": _escape_elisp(org_id),
                    "action": _escape_elisp(action),
                    "tag": _escape_elisp(tag),
                    "files": files_quoted,
                },
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error modifying tags: {e}")
            return False, str(e)

    @staticmethod
    def modify_property(
        org_id: str, action: str, key: str, value: str = ""
    ) -> tuple[bool, str]:
        """
        Set or remove a property on a task.

        Args:
            org_id: The Org ID of the task
            action: "set" or "remove"
            key: Property name
            value: Property value (ignored for remove)

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "modify-property.el",
                params={
                    "org_id": _escape_elisp(org_id),
                    "action": _escape_elisp(action),
                    "key": _escape_elisp(key),
                    "value": _escape_elisp(value),
                    "files": files_quoted,
                },
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error modifying property: {e}")
            return False, str(e)

    @staticmethod
    def add_child_task(parent_org_id: str, title: str) -> Optional[str]:
        """
        Add a child TODO task under a parent heading.

        Args:
            parent_org_id: The Org ID of the parent task
            title: Title for the new child task

        Returns:
            The Org ID of the created child task, or None on error.
            Returns "NOT_FOUND" if the parent task was not found.
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "add-child-task.el",
                params={
                    "parent_org_id": _escape_elisp(parent_org_id),
                    "title": _escape_elisp(title),
                    "files": files_quoted,
                },
            )
            if not result:
                return None
            return result.strip()
        except Exception as e:
            logging.error(f"Error adding child task: {e}")
            return None

    @staticmethod
    def get_parent_task(org_id: str) -> Optional["Task"]:
        """
        Get the parent heading of a task by Org ID.

        Args:
            org_id: The Org ID of the child task

        Returns:
            Task object for the parent, or None if no parent or task not found.
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "get-parent.el", params={"org_id": org_id, "files": files_quoted}
            )
            if not result or result.strip() in ("NOT_FOUND", "NO_PARENT"):
                return None

            parsed = parse_org_ql_result(result)
            if not isinstance(parsed, dict):
                return None

            return Task(
                org_id=parsed.get("id") or "",
                title=parsed.get("heading", ""),
                tags=parsed.get("tags", []),
                location=parsed.get("filename", "") + ":" + str(parsed.get("linenumber", "")),
                status=parsed.get("todo"),
                priority=parsed.get("priority"),
                deadline=parsed.get("deadline"),
                scheduled=parsed.get("scheduled"),
                properties=parsed.get("properties", {}),
            )
        except Exception as e:
            logging.error(f"Error getting parent task: {e}")
            return None

    @staticmethod
    def add_note(org_id: str, note: str) -> tuple[bool, str]:
        """
        Add a timestamped note to a task.

        Args:
            org_id: The Org ID of the task
            note: The note text to append

        Returns:
            Tuple of (success, message)
        """
        from org_warrior import config
        from org_warrior.elisp_helpers import emacs_run_elisp_file

        files = config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        try:
            result = emacs_run_elisp_file(
                "add-note.el",
                params={
                    "org_id": _escape_elisp(org_id),
                    "note": _escape_elisp(note),
                    "files": files_quoted,
                },
            )
            if not result:
                return False, "No response from Emacs"
            if result == "NOT_FOUND":
                return False, "Task not found"
            if result.startswith("ERROR:"):
                return False, result[6:].strip()
            return True, result.strip()
        except Exception as e:
            logging.error(f"Error adding note: {e}")
            return False, str(e)


@dataclass
class Task:
    """Represents a single org-mode task."""

    org_id: str
    title: str
    tags: list[str]
    location: str
    status: Optional[str] = "TODO"
    body: Optional[str] = None
    priority: Optional[str] = None
    scheduled: Optional[datetime] = None
    deadline: Optional[datetime] = None
    handle: Optional[str] = None
    properties: dict = field(default_factory=dict)

    def is_done(self) -> bool:
        """Check if task is completed."""
        return self.status == "DONE"

    def __str__(self) -> str:
        return f"{self.title} [{', '.join(self.tags)}] ({self.location})"
