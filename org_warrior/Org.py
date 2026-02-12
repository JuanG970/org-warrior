from dataclasses import dataclass
from typing import Optional, Tuple
from datetime import datetime, date
import logging
import os
import re


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


def parse_org_ql_result(line: str) -> dict:
    """Parse a single result line from org-ql output."""
    if line.startswith("(") and line.endswith(")"):
        tokens = _tokenize_sexp(line[1:-1])
        if len(tokens) >= 8:
            heading, todo, priority, deadline, scheduled, filepath, lineno, org_id = (
                tokens[:8]
            )
            tags = tokens[8] if len(tokens) > 8 else []
            # Handle tags as a list if it's a nested structure
            if isinstance(tags, str) and tags.startswith("("):
                tags = _tokenize_sexp(tags[1:-1]) if tags != "()" else []
            return {
                "heading": heading or "",
                "todo": todo,
                "priority": priority if priority and priority != "B" else None,
                "deadline": deadline,
                "scheduled": scheduled,
                "file": filepath,
                "line": lineno if isinstance(lineno, int) else None,
                "id": org_id,
                "tags": tags if isinstance(tags, list) else [],
            }
    return {"heading": line, "tags": []}


class OrgQL:
    """
    org-ql query builder and executor.
    """

    @staticmethod
    def build_query(query: str) -> str:
        """
        Build org-ql query from TaskWarrior-like filters.
        """
        if query.strip() == "":
            return "(and (todo) (not (done)))"

        args = query.split()
        logging.debug(f"Parsing query args: {args}")

        conditions = []
        for arg in args:
            if arg.startswith("+"):
                conditions.append(f'(tags "{_escape_elisp(arg[1:])}")')
            elif arg.startswith("project:"):
                conditions.append('(ancestors (tags "project"))')
            elif arg.startswith("pri:") or arg.startswith("priority:"):
                pri = arg.split(":")[1].upper()
                conditions.append(f'(priority "{_escape_elisp(pri)}")')
            elif arg.startswith("due:"):
                due = arg.split(":")[1]
                if due == "today":
                    conditions.append("(deadline :on today)")
                elif due == "week":
                    conditions.append("(deadline :from today :to +7)")
                elif due == "overdue":
                    conditions.append("(deadline :to -1)")
            elif arg.startswith("scheduled:"):
                sched = arg.split(":")[1]
                if sched == "today":
                    conditions.append("(scheduled :on today)")
            else:
                conditions.append(f'(regexp "{_escape_elisp(arg)}")')

        if len(conditions) == 1:
            return conditions[0]
        return f"(and {' '.join(conditions)})"

    @staticmethod
    def get_task_by_id(org_id: str) -> Optional["Task"]:
        """Get a single task by its Org ID."""
        from org_warrior import config
        from org_warrior.EmacsHandler import EmacsHandler

        # Use property query for fast lookup
        query = f'(property "ID" "{_escape_elisp(org_id)}")'

        files = resolve_org_files(config.ORG_FILES)
        files_quoted = " ".join(f'"{f}"' for f in files)

        select_expr = """(list (substring-no-properties (org-get-heading t t t t))
                              (org-entry-get nil "TODO")
                              (org-entry-get nil "PRIORITY")
                              (org-entry-get nil "DEADLINE")
                              (org-entry-get nil "SCHEDULED")
                              (buffer-file-name)
                              (line-number-at-pos)
                              (org-entry-get nil "ID")
                              (org-get-tags))"""

        elisp = f"""(progn
  (require 'org-ql)
  (let ((results (org-ql-select '({files_quoted})
                   '{query}
                   :action '{select_expr})))
    (if results
        (let ((r (car results)))
          (if (listp r)
              (format "%S" r)
            (format "%s" r)))
      "")))
"""

        handler = EmacsHandler(daemon=config.EMACS_SERVER)
        try:
            result = handler.client.eval(elisp)
            if not result or result.strip() == '""':
                return None

            parsed = parse_org_ql_result(result)
            if parsed.get("id"):
                return Task(
                    org_id=parsed["id"],
                    title=parsed["heading"],
                    tags=parsed.get("tags", []),
                    location=f"{parsed.get('file', '')}:{parsed.get('line', 0)}",
                    status=parsed.get("todo"),
                    priority=parsed.get("priority"),
                    deadline=parsed.get("deadline"),
                    scheduled=parsed.get("scheduled"),
                )
            return None
        except Exception as e:
            logging.error(f"Error getting task by ID: {e}")
            return None

    @staticmethod
    def get_task_body(org_id: str) -> Optional[str]:
        """Get the body content of a task by its Org ID."""
        from org_warrior import config
        from org_warrior.EmacsHandler import EmacsHandler

        elisp = f"""(progn
  (let ((m (org-id-find "{_escape_elisp(org_id)}" t)))
    (if (not m)
        "NOT_FOUND"
      (with-current-buffer (marker-buffer m)
        (goto-char m)
        (org-back-to-heading t)
        (end-of-line)
        (forward-char 1)
        (let* ((cb (point))
               (ce (save-excursion (org-end-of-subtree t t) (point))))
          (if (and cb ce (< cb ce))
              (string-trim-right (buffer-substring-no-properties cb ce))))))))
"""

        handler = EmacsHandler(daemon=config.EMACS_SERVER)
        try:
            result = handler.client.eval(elisp)
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
        from org_warrior.EmacsHandler import EmacsHandler
        from org_warrior.HandleCache import HandleCache

        files = files or config.ORG_FILES
        file_list = resolve_org_files(files)
        files_quoted = " ".join(f'"{f}"' for f in file_list)

        logging.debug(f"Running org-ql query: {query_str}")
        logging.debug(f"On files: {files_quoted}")

        # Build the select expression to get full task data
        select_expr = """(list (substring-no-properties (org-get-heading t t t t))
                              (org-entry-get nil "TODO")
                              (org-entry-get nil "PRIORITY")
                              (org-entry-get nil "DEADLINE")
                              (org-entry-get nil "SCHEDULED")
                              (buffer-file-name)
                              (line-number-at-pos)
                              (org-entry-get nil "ID")
                              (org-get-tags))"""

        elisp = f"""(progn
  (require 'org-ql)
  (let ((results (org-ql-select '({files_quoted})
                   '{query_str}
                   :action '{select_expr})))
    (mapconcat (lambda (r)
                 (if (listp r)
                     (format "%S" r)
                   (format "%s" r)))
               results
               "\\n")))
"""

        handler = EmacsHandler(daemon=config.EMACS_SERVER)
        # Use emacs eval directly instead of loading a file
        try:
            result = handler.client.eval(elisp)
            if not result:
                return []

            # Parse each line into a Task
            tasks = []
            handle_cache = HandleCache() if assign_handles else None

            for line in result.splitlines():
                if not line.strip():
                    continue
                parsed = parse_org_ql_result(line)
                if parsed.get("id"):
                    handle = None
                    if handle_cache:
                        handle = handle_cache.get_handle(parsed["id"])

                    task = Task(
                        org_id=parsed["id"],
                        title=parsed["heading"],
                        tags=parsed.get("tags", []),
                        location=f"{parsed.get('file', '')}:{parsed.get('line', 0)}",
                        status=parsed.get("todo"),
                        priority=parsed.get("priority"),
                        deadline=parsed.get("deadline"),
                        scheduled=parsed.get("scheduled"),
                        handle=handle,
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
        from org_warrior import config
        from org_warrior.EmacsHandler import EmacsHandler

        # Escape the title for elisp
        escaped_title = _escape_elisp(title)
        escaped_file = inbox_file  # File path doesn't need escaping
        escaped_heading = _escape_elisp(inbox_heading)

        elisp = f"""(progn
  (require 'org)
  (require 'org-id)
  (let* ((file (expand-file-name "{escaped_file}"))
         (heading "{escaped_heading}"))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\\\*+ %s\\\\b" (regexp-quote heading)) nil t)
          (goto-char (point-max))
          (insert (format "* %s\\n" heading)))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\\n"))
        (let ((pos (point)))
          (insert (format "** TODO %s\\n" "{escaped_title}"))
          (goto-char pos)
          (org-id-get-create)
          (save-buffer)
          (org-id-get))))))
"""

        handler = EmacsHandler(daemon=config.EMACS_SERVER)
        try:
            result = handler.client.eval(elisp)
            if result:
                return result.strip()
            return None
        except Exception as e:
            logging.error(f"Error adding task: {e}")
            return None


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

    def is_done(self) -> bool:
        """Check if task is completed."""
        return self.status == "DONE"

    def __str__(self) -> str:
        return f"{self.title} [{', '.join(self.tags)}] ({self.location})"
