"""Task formatting and display utilities."""

from typing import Optional
from datetime import date, datetime
import csv
import io
import json
import os
from rich.console import Console
from rich.table import Table

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
    def task_to_dict(
        task: Task,
        idx: int = 0,
        show_ids: bool = False,
        show_handles: bool = True,
        show_file: bool = False,
    ) -> dict:
        """Serialize a Task to a plain dict (for JSON/CSV export)."""
        d: dict = {"#": idx}
        if show_handles:
            d["handle"] = task.handle or ""
        d["status"] = task.status or ""
        d["priority"] = task.priority or ""
        d["title"] = task.title
        d["tags"] = ",".join(task.tags) if task.tags else ""
        d["due"] = TaskFormatter._plain_date(task.deadline)
        d["scheduled"] = TaskFormatter._plain_date(task.scheduled)
        if show_ids:
            d["id"] = task.org_id or ""
        if show_file:
            filepath = task.location.split(":")[0] if task.location else ""
            d["file"] = os.path.basename(filepath) if filepath else ""
        return d

    def format_date(self, value, kind: str = "DUE") -> str:
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
            self.console.print(json.dumps(rows, indent=2))
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

        # Add columns
        table.add_column("#", style="dim", no_wrap=True)
        if show_handles:
            table.add_column("Handle", style="dim", no_wrap=True)
        table.add_column("Status", no_wrap=True)
        table.add_column("Pri", style="yellow", no_wrap=True)
        table.add_column("Title", style="bold")
        table.add_column("Tags", style="magenta")
        table.add_column("Due", style="red")
        table.add_column("Scheduled", style="blue")
        if show_ids:
            table.add_column("ID", style="dim")
        if show_file:
            table.add_column("File", style="dim")

        # Add rows
        for idx, task in enumerate(display_tasks, 1):
            row = [str(idx)]

            if show_handles:
                row.append(task.handle or "")

            # Status with color coding
            status = task.status or ""
            color = STATUS_COLORS.get(status, "cyan")
            row.append(f"[{color}]{status}[/{color}]")

            row.append(task.priority or "")
            row.append(task.title)

            # Tags
            row.append(",".join(task.tags) if task.tags else "")

            # Format dates
            row.append(self.format_date(task.deadline, "DUE"))

            if task.scheduled:
                if isinstance(task.scheduled, (datetime, date)):
                    sched_str = (
                        task.scheduled.strftime("%Y-%m-%d")
                        if isinstance(task.scheduled, datetime)
                        else task.scheduled.isoformat()
                    )
                else:
                    sched_str = str(task.scheduled)
                row.append(sched_str)
            else:
                row.append("")

            if show_ids:
                row.append(task.org_id[:8] if task.org_id else "")

            if show_file:
                filepath = task.location.split(":")[0] if task.location else ""
                row.append(os.path.basename(filepath) if filepath else "")

            table.add_row(*row)

        self.console.print(table)

        if limit and limit < total:
            self.console.print(f"\n[dim]Showing {limit} of {total} task(s)[/dim]")
        else:
            self.console.print(f"\n[dim]{total} task(s)[/dim]")

    def format_task_detail(self, task: Task, idx: Optional[int] = None) -> str:
        """Format a task with detailed information."""
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

        return "\n".join(lines)

    def print_properties(self, task, output_format="table"):
        """Print all properties of a task. As a pretty table, or with the specified format."""
        props = task.properties

        if output_format == "json":
            self.console.print(json.dumps(props, indent=2))
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
