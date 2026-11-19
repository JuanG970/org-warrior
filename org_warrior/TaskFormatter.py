"""Task formatting and display utilities."""

from typing import Optional
from datetime import date, datetime
import csv
import io
import json
import os
from collections import namedtuple
from rich.console import Console
from rich.table import Table
from rich.syntax import Syntax

from org_warrior.Org import Task, parse_org_timestamp, date_status

# Valid output formats
OUTPUT_FORMATS = ("table", "json", "csv")


# Sorting keys for task lists
SORT_KEYS = {
    "priority": lambda t: ({"A": 0, "B": 1, "C": 2}.get(t.priority or "B", 1)),
    "due": lambda t: (t.deadline or date.max).isoformat()
    if isinstance(t.deadline, (date, datetime))
    else date.max.isoformat(),
    "scheduled": lambda t: (t.scheduled or date.max).isoformat()
    if isinstance(t.scheduled, (date, datetime))
    else date.max.isoformat(),
    "heading": lambda t: (t.title or "").lower(),
}


# Color mapping for TODO states
STATUS_COLORS = {
    "TODO": "white",
    "STRT": "green bold",
    "WAIT": "yellow",
    "HOLD": "magenta",
    "DONE": "dim",
    "CNCL": "dim strike",
    "KILL": "dim strike",
}


# Ordered list schema for JSON/CSV keys and Rich table columns; see README.
ListField = namedtuple("ListField", "key header style no_wrap in_table")


def _field(key, header, style="", no_wrap=False, in_table=True):
    return ListField(key, header, style, no_wrap, in_table)


LIST_FIELDS = (
    _field("index", "#", "dim", True), _field("handle", "Handle", "dim", True),
    _field("status", "Status"), _field("priority", "Pri", "yellow", True),
    _field("title", "Title", "bold"), _field("tags", "Tags", "magenta"),
    _field("due", "Due", "red"), _field("scheduled", "Scheduled", "blue"),
    _field("id", "ID", "dim"), _field("file", "File", "dim"),
    _field("properties", "", in_table=False), _field("body", "", in_table=False),
)


class TaskFormatter:
    """Handles formatting and displaying tasks."""

    def __init__(self, console: Optional[Console] = None):
        self.console = console or Console()

    @staticmethod
    def _plain_date(value) -> str:
        """Return a plain YYYY-MM-DD date string without Rich markup or org syntax."""
        if not value:
            return ""
        if isinstance(value, datetime):
            return value.strftime("%Y-%m-%d")
        if isinstance(value, date):
            return value.isoformat()
        # Parse org timestamp strings like "<2026-02-16 Mon +1w -1d>"
        parsed, _ = parse_org_timestamp(str(value))
        if parsed:
            return parsed.isoformat()
        return str(value)

    @staticmethod
    def _present(key, task, idx, show_ids, show_handles, show_file) -> bool:
        flags = {"index": idx is not None, "handle": show_handles, "id": show_ids,
                 "file": show_file, "body": bool(task.body)}
        return flags.get(key, True)

    @staticmethod
    def _value(key, task, idx, table: bool = False):
        """Value for one schema field, native for dicts and rendered for tables."""
        if key == "index":
            return str(idx) if table else idx
        if key == "status":
            status = task.status or ""
            if table and status:
                color = STATUS_COLORS.get(status, "cyan")
                return f"[{color}]{status}[/{color}]"
            return status
        if key == "due":
            if table:
                return TaskFormatter.format_date(task.deadline, "DUE")
            return TaskFormatter._plain_date(task.deadline)
        if key == "scheduled":
            return TaskFormatter._plain_date(task.scheduled)
        if key == "id":
            return (task.org_id or "")[:8] if table else (task.org_id or "")
        if key == "file":
            path = (task.location or "").split(":")[0]
            return os.path.basename(path) if path else ""
        if key == "properties":
            return task.properties or {}
        if key == "tags":
            return ",".join(task.tags) if task.tags else ""
        return getattr(task, key) or ""

    @staticmethod
    def task_to_dict(
        task: Task,
        idx,
        show_ids: bool = False,
        show_handles: bool = True,
        show_file: bool = False,
    ) -> dict:
        """Serialize a Task to a plain dict; keys come from ``LIST_FIELDS``."""
        return {
            f.key: TaskFormatter._value(f.key, task, idx)
            for f in LIST_FIELDS if TaskFormatter._present(f.key, task, idx, show_ids, show_handles, show_file)
        }

    @staticmethod
    def format_date(value, kind: str = "DUE") -> str:
        """Format a date value with color coding."""
        if not value:
            return ""

        if isinstance(value, (datetime, date)):
            due_str = (
                value.strftime("%Y-%m-%d")
                if isinstance(value, datetime)
                else value.isoformat()
            )
            due_date = value.date() if isinstance(value, datetime) else value
        else:
            due_str = str(value)
            due_date, _ = parse_org_timestamp(due_str)

        status = date_status(due_date)
        if kind == "DUE":
            if status == "overdue":
                return f"[red bold]{due_str}[/red bold]"
            elif status == "today":
                return f"[yellow bold]{due_str}[/yellow bold]"
            else:
                return f"[green]{due_str}[/green]"
        else:  # Scheduled
            if status == "overdue":
                return f"[red]{due_str}[/red]"
            elif status == "today":
                return f"[cyan]{due_str}[/cyan]"
            else:
                return f"[blue]{due_str}[/blue]"

    def print_tasks(
        self,
        tasks: list[Task],
        show_ids: bool = False,
        show_handles: bool = True,
        limit: Optional[int] = None,
        sort_key: Optional[str] = None,
        show_file: bool = False,
        output_format: str = "table",
    ):
        """Print tasks in a formatted table, JSON, or CSV."""
        if not tasks:
            if output_format == "json":
                self.console.print("[]")
            elif output_format == "csv":
                pass  # empty CSV is fine
            else:
                self.console.print("[yellow]No tasks found.[/yellow]")
            return

        # Apply sorting
        if sort_key and sort_key in SORT_KEYS:
            tasks.sort(key=SORT_KEYS[sort_key])

        # Apply limit
        display_tasks = tasks[:limit] if limit else tasks
        total = len(tasks)

        # ---- JSON output ----
        if output_format == "json":
            rows = [
                self.task_to_dict(task, idx, show_ids, show_handles, show_file)
                for idx, task in enumerate(display_tasks, 1)
            ]
            self.console.file.write(json.dumps(rows, indent=2) + "\n")
            self.console.file.flush()
            return

        # ---- CSV output ----
        if output_format == "csv":
            rows = [
                self.task_to_dict(task, idx, show_ids, show_handles, show_file)
                for idx, task in enumerate(display_tasks, 1)
            ]
            if rows:
                buf = io.StringIO()
                writer = csv.DictWriter(buf, fieldnames=rows[0].keys())
                writer.writeheader()
                writer.writerows(rows)
                self.console.print(buf.getvalue().rstrip())
            return

        # ---- Rich table output ----
        table = Table(title="Tasks")

        columns = [
            f for f in LIST_FIELDS
            if f.in_table and self._present(f.key, display_tasks[0], 1, show_ids, show_handles, show_file)
        ]
        for f in columns:
            table.add_column(f.header, style=f.style or None, no_wrap=f.no_wrap)
        for idx, task in enumerate(display_tasks, 1):
            table.add_row(
                *[str(self._value(f.key, task, idx, True)) for f in columns]
            )

        self.console.print(table)

        if limit and limit < total:
            self.console.print(f"\n[dim]Showing {limit} of {total} task(s)[/dim]")
        else:
            self.console.print(f"\n[dim]{total} task(s)[/dim]")

    def format_task_detail(self, task: Task, idx: Optional[int] = None, output_format="table") -> Optional[str]:
        """Format a task with detailed information and return the rendered text."""
        if output_format == "json":
            self.console.file.write(json.dumps(self.task_to_dict(task, idx, show_ids=True, show_handles=True), indent=2) + "\n")
            self.console.file.flush()
            return None
        lines = []

        # Header
        prefix = f"Task {idx}:" if idx is not None else "Task:"
        lines.append(prefix)

        # Heading with status and priority
        heading = task.title or ""
        if task.status:
            lines.append(
                f"  [cyan bold][{task.status}][/cyan bold] [bold]{heading}[/bold]"
            )
        else:
            lines.append(f"  [bold]{heading}[/bold]")

        # Priority on separate line if present
        if task.priority:
            lines.append(f"  Priority: [yellow bold]({task.priority})[/yellow bold]")

        # Deadline
        if task.deadline:
            formatted_deadline = self.format_date(task.deadline, "DUE")
            lines.append(f"  Deadline: {formatted_deadline}")

        # Scheduled
        if task.scheduled:
            formatted_scheduled = self.format_date(task.scheduled, "SCH")
            lines.append(f"  Scheduled: {formatted_scheduled}")

        # Location
        if task.location:
            lines.append(f"  Location: [dim]{task.location}[/dim]")

        # Handle
        if task.handle:
            lines.append(f"  Handle: {task.handle}")

        # Org ID
        if task.org_id:
            lines.append(f"  ID: {task.org_id}")

        for l in lines:
            self.console.print(l)
        # Body
        if task.body:
            self.console.print("\n  [bold]Body:[/bold]")
            body_syntax = Syntax(task.body, "org", theme="monokai", word_wrap=True)
            self.console.print(body_syntax)
        return "\n".join(lines)


    def print_properties(self, task, output_format="table"):
        """Print all properties of a task. As a pretty table, or with the specified format."""
        props = task.properties

        if output_format == "json":
            self.console.file.write(json.dumps(props, indent=2) + "\n")
            self.console.file.flush()
        elif output_format == "csv":
            buf = io.StringIO()
            writer = csv.DictWriter(buf, fieldnames=props.keys())
            writer.writeheader()
            writer.writerow(props)
            self.console.print(buf.getvalue().rstrip())
        else:
            table = Table(title="Task Properties")
            table.add_column("Property", style="dim", no_wrap=True)
            table.add_column("Value", style="bold")
            for key, value in props.items():
                table.add_row(key, str(value))
            self.console.print(table)
