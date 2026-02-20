"""Main CLI interface for org-warrior."""

import logging
import sys
from typing import Optional
import typer
from rich.console import Console

from org_warrior import config
from org_warrior.Org import OrgQL
from org_warrior.TaskFormatter import TaskFormatter, OUTPUT_FORMATS


app = typer.Typer()
modify_app = typer.Typer(help="Modify task tags or properties.")
tag_app = typer.Typer(help="Add or remove tags on a task.")
modify_app.add_typer(tag_app, name="tag")
app.add_typer(modify_app, name="modify")
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
        help="Filter: +tag, state:TODO, pri:A, due:today, scheduled:today, project:any, or text search.",
    ),
    raw: Optional[str] = typer.Option(
        None, "--raw", help="Raw org-ql s-expression (bypasses filter parsing)"
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
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """List all tasks matching the given filter."""
    try:
        if raw:
            org_ql_filter = raw
        else:
            org_ql_filter = OrgQL.build_query(query)
        logging.debug(f"org-ql filter: {org_ql_filter}")

        if output_format not in OUTPUT_FORMATS:
            err_console.print(
                f"[red]Error: unknown format '{output_format}'. Choose from: {', '.join(OUTPUT_FORMATS)}[/red]"
            )
            return 1

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
            output_format=output_format,
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
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    )
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
    
    # Get the body if requested
    if not no_body:
        try:
            body = OrgQL.get_task_body(org_id)
            task.body = body
        except RuntimeError as e:
            err_console.print(f"[red]Error reading body: {e}[/red]")
            return 1
    # Format and display task details
    try:
        formatter.format_task_detail(task, output_format=output_format)
        return 0
    except Exception as e:
        err_console.print(f"[red]Error displaying task: {e}[/red]")
        logging.exception("Error in show command")
        return 1



def _create_child_task(parent_id: str, title: str, no_git: bool) -> None:
    """Shared implementation for creating a child task under a parent.

    Raises:
        RuntimeError: on any failure (empty title, parent not found, emacs error)
    """
    from org_warrior.HandleCache import HandleCache

    if not title.strip():
        raise RuntimeError("task title cannot be empty")

    with HandleCache() as handle_cache:
        parent_org_id = handle_cache.resolve(parent_id)
    if not parent_org_id:
        raise RuntimeError(f"cannot resolve parent: {parent_id}")

    result = OrgQL.add_child_task(parent_org_id, title)
    if not result:
        raise RuntimeError("failed to create child task")
    if result == "NOT_FOUND":
        raise RuntimeError(f"parent task not found: {parent_id}")
    if result.startswith("ERROR:"):
        raise RuntimeError(result[6:].strip())

    child_org_id = result.strip('"')
    with HandleCache() as handle_cache:
        handle = handle_cache.get_handle(child_org_id)

    if not no_git:
        config.git_commit_org(f"org-warrior: add {child_org_id} under {parent_org_id}")

    console.print(f"[green]Created:[/green] {handle} (ID: {child_org_id})")


@app.command()
def add(
    title: str = typer.Argument(..., help="Title of the new task"),
    parent: Optional[str] = typer.Option(
        None, "--parent", "-p", help="Parent task ID (Org ID or handle) — adds as subtask"
    ),
    inbox_file: str = typer.Option(
        "~/org/inbox.org", "--inbox-file", help="Inbox file path"
    ),
    inbox_heading: str = typer.Option("Inbox", "--inbox-heading", help="Inbox heading"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Add a new TODO task to the inbox, or as a subtask under a parent."""
    from org_warrior.HandleCache import HandleCache

    if parent:
        try:
            _create_child_task(parent, title, no_git)
        except RuntimeError as e:
            err_console.print(f"[red]Error: {e}[/red]")
            raise typer.Exit(code=1)
        return

    # Add to inbox (default)
    org_id = OrgQL.add_task(title, inbox_file, inbox_heading)
    if not org_id:
        err_console.print("[red]Failed to create task[/red]")
        return 1

    with HandleCache() as handle_cache:
        handle = handle_cache.get_handle(org_id)

    if not no_git:
        config.git_commit_org(f"org-warrior: add {org_id}")

    console.print(f"[green]Created:[/green] {handle} (ID: {org_id})")
    return 0


def _run_query_command(
    query: str,
    empty_message: str = "No tasks found.",
    limit: Optional[int] = None,
    sort: Optional[str] = None,
    show_file: bool = False,
    output_format: str = "table",
) -> int:
    """Helper to run an org-ql query and display results (DRY)."""
    try:
        if output_format not in OUTPUT_FORMATS:
            err_console.print(
                f"[red]Error: unknown format '{output_format}'. Choose from: {', '.join(OUTPUT_FORMATS)}[/red]"
            )
            return 1

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
            if output_format == "json":
                console.print("[]")
            elif output_format == "csv":
                pass
            else:
                console.print(f"[dim]{empty_message}[/dim]")
            return 0

        formatter.print_tasks(
            tasks,
            show_ids=config.CONFIG_SHOW_IDS,
            show_handles=config.CONFIG_SHOW_HANDLES,
            limit=actual_limit,
            sort_key=actual_sort,
            show_file=show_file,
            output_format=output_format,
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
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show next actionable tasks (STRT or high-priority TODO)."""
    query = '(or (todo "STRT") (and (todo "TODO") (priority "A")))'
    return _run_query_command(
        query, "No next actions found.", limit, sort, show_file, output_format
    )


@app.command()
def today(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show tasks due or scheduled for today (including overdue)."""
    query = "(and (not (done)) (or (deadline :to today) (scheduled :to today) (ts-active :on today)))"
    return _run_query_command(
        query, "No tasks for today.", limit, sort, show_file, output_format
    )


@app.command()
def week(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show tasks for the coming week."""
    query = "(and (not (done)) (or (deadline :from today :to +7) (scheduled :from today :to +7)))"
    return _run_query_command(
        query, "No tasks for this week.", limit, sort, show_file, output_format
    )


@app.command()
def overdue(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show overdue tasks."""
    query = "(and (not (done)) (deadline :to -1))"
    return _run_query_command(
        query, "No overdue tasks.", limit, sort, show_file, output_format
    )


@app.command()
def done(
    limit: Optional[int] = typer.Option(
        None, "--limit", help="Limit number of results"
    ),
    sort: Optional[str] = typer.Option(
        None, "--sort", help="Sort by: priority, due, scheduled, heading"
    ),
    show_file: bool = typer.Option(False, "--show-file", help="Show source filename"),
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show recently completed tasks."""
    query = "(done)"
    return _run_query_command(
        query, "No completed tasks found.", limit, sort, show_file, output_format
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
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show active projects (headings with 'project' tag)."""
    query = '(and (tags "project") (not (done)))'
    return _run_query_command(
        query, "No active projects found.", limit, sort, show_file, output_format
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
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show tasks waiting on something (WAIT state or 'waiting' tag)."""
    query = '(or (todo "WAIT") (tags "waiting"))'
    return _run_query_command(
        query, "No waiting tasks found.", limit, sort, show_file, output_format
    )


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


@tag_app.command(name="add")
def tag_add(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    tag: str = typer.Argument(..., help="Tag name to add"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Add a tag to a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    if not tag:
        err_console.print("[red]Error: tag name cannot be empty[/red]")
        raise typer.Exit(code=1)

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            raise typer.Exit(code=1)

    # Modify tag
    success, message = OrgQL.modify_tags(org_id, "add", tag)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        raise typer.Exit(code=1)

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: tag add {tag} {org_id}")

    console.print(f"[green]Tag added:[/green] {message}")


@tag_app.command(name="remove")
def tag_remove(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    tag: str = typer.Argument(..., help="Tag name to remove"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Remove a tag from a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    if not tag:
        err_console.print("[red]Error: tag name cannot be empty[/red]")
        raise typer.Exit(code=1)

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            raise typer.Exit(code=1)

    # Modify tag
    success, message = OrgQL.modify_tags(org_id, "remove", tag)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        raise typer.Exit(code=1)

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: tag remove {tag} {org_id}")

    console.print(f"[green]Tag removed:[/green] {message}")

@app.command(name="properties")
def view_properties(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    )
):
    """Get the properties on a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            raise typer.Exit(code=1)

    try:
        task = OrgQL.get_task_by_id(org_id)
        if not task:
            err_console.print(f"[red]No task found with ID: {org_id}[/red]")
            raise typer.Exit(code=1)
        formatter.print_properties(task, output_format)
    except Exception as e:
        err_console.print(f"[red]Error retrieving task: {e}[/red]")
        raise typer.Exit(code=1)

@modify_app.command(name="property")
def modify_property(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    key: str = typer.Argument(..., help="Property name"),
    value: Optional[str] = typer.Argument(None, help="Property value (omit to remove)"),
    remove: bool = typer.Option(
        False, "--remove", "-r", help="Remove the property instead of setting it"
    ),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Set or remove a property on a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    # Determine action
    if remove:
        action = "remove"
        prop_value = ""
    elif value is not None:
        action = "set"
        prop_value = value
    else:
        err_console.print(
            "[red]Error: provide a value to set, or use --remove to delete the property[/red]"
        )
        raise typer.Exit(code=1)

    if not key:
        err_console.print("[red]Error: property name cannot be empty[/red]")
        raise typer.Exit(code=1)

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            raise typer.Exit(code=1)

    # Modify property
    success, message = OrgQL.modify_property(org_id, action, key, prop_value)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        raise typer.Exit(code=1)

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: modify property {action} {key} {org_id}")

    console.print(f"[green]Property modified:[/green] {message}")


@app.command(name="add-child")
def add_child(
    parent_id: str = typer.Argument(..., help="Parent task ID (Org ID or handle)"),
    title: str = typer.Argument(..., help="Title of the new child task"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Add a child TODO task under an existing task (alias for: add --parent PARENT TITLE)."""
    try:
        _create_child_task(parent_id, title, no_git)
    except RuntimeError as e:
        err_console.print(f"[red]Error: {e}[/red]")
        raise typer.Exit(code=1)


@app.command(name="get-parent")
def get_parent(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    output_format: str = typer.Option(
        "table", "--format", "-f", help="Output format: table, json, csv"
    ),
):
    """Show the parent task of a given task."""
    from org_warrior.HandleCache import HandleCache

    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            raise typer.Exit(code=1)

    parent = OrgQL.get_parent_task(org_id)
    if not parent:
        err_console.print(f"[yellow]No parent found for: {task_id}[/yellow]")
        raise typer.Exit(code=1)

    if parent.org_id:
        with HandleCache() as handle_cache:
            parent.handle = handle_cache.get_handle(parent.org_id)

    try:
        formatter.format_task_detail(parent, output_format=output_format)
    except Exception as e:
        err_console.print(f"[red]Error displaying parent: {e}[/red]")
        logging.exception("Error in get-parent command")
        raise typer.Exit(code=1)


@app.command()
def note(
    task_id: str = typer.Argument(..., help="Task ID (Org ID or handle)"),
    text: str = typer.Argument(..., help="Note text to append to the task"),
    no_git: bool = typer.Option(False, "--no-git", help="Don't auto-commit to git"),
):
    """Add a timestamped note to a task."""
    from org_warrior.HandleCache import HandleCache
    from org_warrior import config

    if not text.strip():
        err_console.print("[red]Error: note text cannot be empty[/red]")
        raise typer.Exit(code=1)

    # Resolve handle to Org ID
    with HandleCache() as handle_cache:
        org_id = handle_cache.resolve(task_id)
        if not org_id:
            raise typer.Exit(code=1)

    # Add note
    success, message = OrgQL.add_note(org_id, text)
    if not success:
        err_console.print(f"[red]Error: {message}[/red]")
        raise typer.Exit(code=1)

    # Auto-commit to git if enabled
    if not no_git:
        config.git_commit_org(f"org-warrior: note {org_id}")

    console.print(f"[green]{message}[/green]")


if __name__ == "__main__":
    app()
