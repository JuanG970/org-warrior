"""Task formatting and display utilities."""

from typing import Optional
from datetime import date, datetime
import os
from rich.console import Console
from rich.table import Table

from org_warrior.Org import Task, parse_org_timestamp, date_status


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


class TaskFormatter:
    """Handles formatting and displaying tasks."""

    def __init__(self, console: Optional[Console] = None):
        self.console = console or Console()

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
    ):
        """Print tasks in a formatted table."""
        if not tasks:
            self.console.print("[yellow]No tasks found.[/yellow]")
            return

        # Apply sorting
        if sort_key and sort_key in SORT_KEYS:
            tasks.sort(key=SORT_KEYS[sort_key])

        # Apply limit
        display_tasks = tasks[:limit] if limit else tasks
        total = len(tasks)

        # Create rich table
        table = Table(title="Tasks")

        # Add columns
        table.add_column("#", style="dim", no_wrap=True)
        if show_handles:
            table.add_column("Handle", style="dim", no_wrap=True)
        table.add_column("Status", style="cyan", no_wrap=True)
        table.add_column("Priority", style="yellow", no_wrap=True)
        table.add_column("Title", style="bold")
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

            row.append(task.status or "")
            row.append(task.priority or "")
            row.append(task.title)

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
