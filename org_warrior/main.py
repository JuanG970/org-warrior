"""Main CLI interface for org-warrior."""

import logging
import sys
from typing import Optional
import typer
from rich.console import Console
from rich.syntax import Syntax

from org_warrior import config
from org_warrior.Org import OrgQL
from org_warrior.TaskFormatter import TaskFormatter


app = typer.Typer()
console = Console()
err_console = Console(stderr=True)
formatter = TaskFormatter(console)


@app.callback()
def callback():
    """
    org-warrior - TaskWarrior-like CLI for Org mode
    """
    pass


@app.command()
def list(
    query: str = typer.Argument(
        "",
        help="Filter tasks using TaskWarrior's filter. Defaults to org-ql: (and (todo) (not (done))).",
    ),
    show_ids: bool = typer.Option(False, "--ids", "--show-ids", help="Show Org IDs"),
    no_handles: bool = typer.Option(False, "--no-handles", help="Hide handles"),
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """List all tasks matching the given filter."""
    try:
        # Build org-ql query
        org_ql_filter = OrgQL.build_query(query)
        logging.debug(f"Built org-ql filter: {org_ql_filter}")

        # Run query
        tasks = OrgQL.run_query(org_ql_filter)

        # Use config defaults if flags not provided
        actual_show_ids = show_ids or config.CONFIG_SHOW_IDS
        actual_show_handles = not no_handles and config.CONFIG_SHOW_HANDLES
        actual_limit = limit
        if actual_limit is None and config.DEFAULT_LIMIT:
            try:
                actual_limit = int(config.DEFAULT_LIMIT)
            except ValueError:
                pass
        actual_sort = sort or config.DEFAULT_SORT or None

        # Display results
        formatter.print_tasks(
            tasks,
            show_ids=actual_show_ids,
            show_handles=actual_show_handles,
            limit=actual_limit,
            sort_key=actual_sort,
            show_file=show_file,
        )
        return 0
    except Exception as e:
        err_console.print(f"[red]Error: {e}[/red]")
        logging.exception("Error in list command")
        return 1


@app.command()
def show(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    no_body: bool = typer.Option(False, "--no-body", help="Don't show task body"),
):
    """Show detailed information about a task."""
    from org_warrior.HandleCache import HandleCache

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            return 1

    # Get task by ID
    task = OrgQL.get_task_by_id(org_id)
    if not task:
        err_console.print(f"[red]No task found with ID: {org_id}[/red]")
        return 1

    # Assign handle to the task
    with HandleCache() as handle_cache:
        task.handle = handle_cache.get_handle(org_id)

    # Format and display task details
    detail_output = formatter.format_task_detail(task)
    console.print(detail_output)

    # Get and display body if requested
    if not no_body:
        try:
            body = OrgQL.get_task_body(org_id)
            console.print("\nBody:")
            if body and body.strip():
                body_nice = Syntax(body, "org", theme="monokai", word_wrap=True)
                console.print(body_nice)
            else:
                console.print("[dim](empty)[/dim]")
        except RuntimeError as e:
            err_console.print(f"[red]Error reading body: {e}[/red]")
            return 1

    return 0


@app.command()
def add(
    title: str = typer.Argument(..., help="Title of the new task"),
    inbox_file: str = typer.Option(
        "~/org/inbox.org", "--inbox-file", help="Inbox file path"
    ),
    inbox_heading: str = typer.Option("Inbox", "--inbox-heading", help="Inbox heading"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Add a new TODO task to the inbox."""
    from org_warrior.HandleCache import HandleCache

    # Add the task
    org_id = OrgQL.add_task(title, inbox_file, inbox_heading)
    if not org_id:
        err_console.print("[red]Failed to create task[/red]")
        return 1

    # Generate handle for the new task
    with HandleCache() as handle_cache:
        handle = handle_cache.get_handle(org_id)

    # Auto-commit to git if enabled
    if not no_git:
        from org_warrior import config

        config.git_commit_org(f"org-warrior: add {org_id}")

    # Display success message
    console.print(f"[green]Created:[/green] {handle} (ID: {org_id})")
    return 0


def _run_query_command(
    query: str,
    empty_message: str = "No tasks found.",
    limit: Optional[int] = None,
    sort: Optional[str] = None,
    show_file: bool = False,
) -> int:
    """Helper to run an org-ql query and display results (DRY)."""
    try:
        tasks = OrgQL.run_query(query)

        # Use config defaults if not provided
        actual_limit = limit
        if actual_limit is None and config.DEFAULT_LIMIT:
            try:
                actual_limit = int(config.DEFAULT_LIMIT)
            except ValueError:
                pass
        actual_sort = sort or config.DEFAULT_SORT or None

        if not tasks:
            console.print(f"[dim]{empty_message}[/dim]")
            return 0

        formatter.print_tasks(
            tasks,
            show_ids=config.CONFIG_SHOW_IDS,
            show_handles=config.CONFIG_SHOW_HANDLES,
            limit=actual_limit,
            sort_key=actual_sort,
            show_file=show_file,
        )
        return 0
    except Exception as e:
        err_console.print(f"[red]Error: {e}[/red]")
        logging.exception("Error in query command")
        return 1


@app.command()
def next(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show next actionable tasks (STRT or high-priority TODO)."""
    query = '(or (todo "STRT") (and (todo "TODO") (priority "A")))'
    return _run_query_command(query, "No next actions found.", limit, sort, show_file)


@app.command()
def today(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show tasks due or scheduled for today (including overdue)."""
    query = "(and (not (done)) (or (deadline :to today) (scheduled :to today) (ts-active :on today)))"
    return _run_query_command(query, "No tasks for today.", limit, sort, show_file)


@app.command()
def week(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show tasks for the coming week."""
    query = "(and (not (done)) (or (deadline :from today :to +7) (scheduled :from today :to +7)))"
    return _run_query_command(query, "No tasks for this week.", limit, sort, show_file)


@app.command()
def overdue(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show overdue tasks."""
    query = "(and (not (done)) (deadline :to -1))"
    return _run_query_command(query, "No overdue tasks.", limit, sort, show_file)


@app.command()
def done(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show recently completed tasks."""
    query = "(done)"
    return _run_query_command(
        query, "No completed tasks found.", limit, sort, show_file
    )


@app.command()
def projects(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show active projects (headings with 'project' tag)."""
    query = '(and (tags "project") (not (done)))'
    return _run_query_command(
        query, "No active projects found.", limit, sort, show_file
    )


@app.command()
def waiting(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
):
    """Show tasks waiting on something (WAIT state or 'waiting' tag)."""
    query = '(or (todo "WAIT") (tags "waiting"))'
    return _run_query_command(query, "No waiting tasks found.", limit, sort, show_file)


@app.command()
def start(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Clock in to a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            return 1

    # Clock in
    success, message = OrgQL.clock_in(org_id)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        return 1

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: start {org_id}")

    console.print(f"[green]Clocked in:[/green] {message}")
    return 0


@app.command()
def stop(
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Clock out from the current task."""
    from org_warrior import config

    success, message = OrgQL.clock_out()
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        return 1

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org("org-warrior: stop")

    console.print(f"[green]{message}[/green]")
    return 0


@app.command()
def schedule(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    date: str = typer.Argument(..., help="Date (e.g., 'today', '+1d', '2024-12-25')"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Schedule a task for a specific date."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            return 1

    # Schedule task
    success, message = OrgQL.schedule(org_id, date)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        return 1

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: schedule {org_id} {date}")

    console.print(f"[green]Scheduled:[/green] {message}")
    return 0


@app.command()
def deadline(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    date: str = typer.Argument(..., help="Date (e.g., 'today', '+1d', '2024-12-25')"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Set a deadline for a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            return 1

    # Set deadline
    success, message = OrgQL.deadline(org_id, date)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        return 1

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: deadline {org_id} {date}")

    console.print(f"[green]Deadline set:[/green] {message}")
    return 0


@app.command(name="set-state")
def set_state(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    state: str = typer.Argument(..., help="TODO state (e.g., 'TODO', 'DONE', 'STRT')"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Change the TODO state of a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            return 1

    # Set state
    success, message = OrgQL.set_state(org_id, state)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        return 1

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: set-state {org_id} {state}")

    console.print(f"[green]State changed:[/green] {message}")
    return 0


if __name__ == "__main__":
    app()
