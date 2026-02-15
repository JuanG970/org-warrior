"""Tests for TaskFormatter module."""

import json
import pytest
from datetime import datetime, date, timedelta
from org_warrior.Org import Task
from org_warrior.TaskFormatter import (
    TaskFormatter,
    SORT_KEYS,
    STATUS_COLORS,
    OUTPUT_FORMATS,
)
from rich.console import Console
from io import StringIO


def _make_task(**kwargs) -> Task:
    """Helper to create a Task with defaults."""
    defaults = {
        "org_id": "test-id",
        "title": "Test Task",
        "tags": [],
        "location": "/path/to/file.org:10",
        "status": "TODO",
    }
    defaults.update(kwargs)
    return Task(**defaults)


class TestTaskFormatter:
    """Tests for TaskFormatter class."""

    def setup_method(self):
        """Set up test fixtures."""
        # Use StringIO to capture output
        self.output = StringIO()
        self.console = Console(file=self.output, force_terminal=False)
        self.formatter = TaskFormatter(self.console)

    def test_format_task_detail_minimal(self):
        """Test formatting a task with minimal information."""
        task = _make_task(org_id="test-id-001", location="/path/to/file.org:10")
        result = self.formatter.format_task_detail(task)

        assert "Task:" in result
        assert "Test Task" in result
        assert "TODO" in result
        assert "test-id-001" in result
        assert "/path/to/file.org:10" in result

    def test_format_task_detail_with_priority(self):
        """Test formatting a task with priority."""
        task = _make_task(title="High Priority Task", priority="A")
        result = self.formatter.format_task_detail(task)

        assert "Priority: " in result
        assert "(A)" in result

    def test_format_task_detail_with_dates(self):
        """Test formatting a task with deadline and scheduled dates."""
        task = _make_task(
            title="Task with Dates",
            deadline=datetime(2026, 2, 20, 10, 30),
            scheduled=datetime(2026, 2, 15, 9, 0),
        )
        result = self.formatter.format_task_detail(task)

        assert "Deadline:" in result
        assert "Scheduled:" in result
        assert "2026-02-" in result

    def test_format_task_detail_with_index(self):
        """Test formatting a task with an index number."""
        task = _make_task(title="Indexed Task")
        result = self.formatter.format_task_detail(task, idx=5)
        assert "Task 5:" in result

    def test_format_task_detail_no_status(self):
        """Test formatting a task without a status."""
        task = _make_task(title="No Status Task", status=None)
        result = self.formatter.format_task_detail(task)
        assert "No Status Task" in result
        assert "Task:" in result

    def test_format_task_detail_with_handle(self):
        """Test formatting a task with a handle."""
        task = _make_task(handle="alpha-bravo-charlie")
        result = self.formatter.format_task_detail(task)
        assert "Handle: alpha-bravo-charlie" in result


class TestFormatDate:
    """Tests for TaskFormatter.format_date."""

    def setup_method(self):
        self.formatter = TaskFormatter(Console(file=StringIO(), force_terminal=False))

    def test_format_date_none(self):
        assert self.formatter.format_date(None) == ""

    def test_format_date_empty_string(self):
        assert self.formatter.format_date("") == ""

    def test_format_date_future_due(self):
        future = date.today() + timedelta(days=30)
        result = self.formatter.format_date(future, "DUE")
        assert "[green]" in result
        assert future.isoformat() in result

    def test_format_date_today_due(self):
        today = date.today()
        result = self.formatter.format_date(today, "DUE")
        assert "[yellow bold]" in result

    def test_format_date_overdue(self):
        past = date.today() - timedelta(days=5)
        result = self.formatter.format_date(past, "DUE")
        assert "[red bold]" in result

    def test_format_date_scheduled_future(self):
        future = date.today() + timedelta(days=3)
        result = self.formatter.format_date(future, "SCH")
        assert "[blue]" in result

    def test_format_date_scheduled_today(self):
        today = date.today()
        result = self.formatter.format_date(today, "SCH")
        assert "[cyan]" in result

    def test_format_date_scheduled_overdue(self):
        past = date.today() - timedelta(days=1)
        result = self.formatter.format_date(past, "SCH")
        assert "[red]" in result

    def test_format_date_datetime_object(self):
        dt = datetime(2030, 6, 15, 10, 30)
        result = self.formatter.format_date(dt, "DUE")
        assert "2030-06-15" in result

    def test_format_date_string_timestamp(self):
        result = self.formatter.format_date("<2020-01-01 Wed>", "DUE")
        assert "2020-01-01" in result
        assert "[red bold]" in result  # overdue


class TestPlainDate:
    """Tests for TaskFormatter._plain_date."""

    def test_none(self):
        assert TaskFormatter._plain_date(None) == ""

    def test_empty_string(self):
        assert TaskFormatter._plain_date("") == ""

    def test_datetime(self):
        assert TaskFormatter._plain_date(datetime(2026, 3, 15, 10, 0)) == "2026-03-15"

    def test_date(self):
        assert TaskFormatter._plain_date(date(2026, 3, 15)) == "2026-03-15"

    def test_org_timestamp_string(self):
        assert TaskFormatter._plain_date("<2026-02-16 Mon +1w -1d>") == "2026-02-16"

    def test_unparseable_string(self):
        assert TaskFormatter._plain_date("not-a-date") == "not-a-date"


class TestTaskToDict:
    """Tests for TaskFormatter.task_to_dict."""

    def test_basic_dict(self):
        task = _make_task(status="STRT", priority="A", handle="alpha-bravo")
        d = TaskFormatter.task_to_dict(task, idx=1)
        assert d["#"] == 1
        assert d["handle"] == "alpha-bravo"
        assert d["status"] == "STRT"
        assert d["priority"] == "A"
        assert d["title"] == "Test Task"

    def test_no_handles(self):
        task = _make_task()
        d = TaskFormatter.task_to_dict(task, idx=1, show_handles=False)
        assert "handle" not in d

    def test_show_ids(self):
        task = _make_task(org_id="abc-123-def")
        d = TaskFormatter.task_to_dict(task, idx=1, show_ids=True)
        assert d["id"] == "abc-123-def"

    def test_show_file(self):
        task = _make_task(location="/home/user/org/work.org:42")
        d = TaskFormatter.task_to_dict(task, idx=1, show_file=True)
        assert d["file"] == "work.org"

    def test_tags_comma_separated(self):
        task = _make_task(tags=["work", "urgent", "email"])
        d = TaskFormatter.task_to_dict(task, idx=1)
        assert d["tags"] == "work,urgent,email"

    def test_empty_tags(self):
        task = _make_task(tags=[])
        d = TaskFormatter.task_to_dict(task, idx=1)
        assert d["tags"] == ""

    def test_date_fields(self):
        task = _make_task(
            deadline=date(2026, 5, 1),
            scheduled=datetime(2026, 4, 20, 9, 0),
        )
        d = TaskFormatter.task_to_dict(task, idx=1)
        assert d["due"] == "2026-05-01"
        assert d["scheduled"] == "2026-04-20"


class TestPrintTasksJSON:
    """Tests for print_tasks with JSON output."""

    def setup_method(self):
        self.output = StringIO()
        self.console = Console(file=self.output, force_terminal=False)
        self.formatter = TaskFormatter(self.console)

    def test_json_output(self):
        tasks = [
            _make_task(title="Task A", org_id="id-a", handle="ha"),
            _make_task(title="Task B", org_id="id-b", handle="hb"),
        ]
        self.formatter.print_tasks(tasks, output_format="json")
        raw = self.output.getvalue()
        data = json.loads(raw)
        assert len(data) == 2
        assert data[0]["title"] == "Task A"
        assert data[1]["title"] == "Task B"

    def test_json_empty(self):
        self.formatter.print_tasks([], output_format="json")
        raw = self.output.getvalue().strip()
        assert raw == "[]"

    def test_json_with_limit(self):
        tasks = [_make_task(title=f"T{i}") for i in range(10)]
        self.formatter.print_tasks(tasks, output_format="json", limit=3)
        data = json.loads(self.output.getvalue())
        assert len(data) == 3


class TestPrintTasksCSV:
    """Tests for print_tasks with CSV output."""

    def setup_method(self):
        self.output = StringIO()
        self.console = Console(file=self.output, force_terminal=False)
        self.formatter = TaskFormatter(self.console)

    def test_csv_output(self):
        tasks = [
            _make_task(title="Task A", handle="ha"),
            _make_task(title="Task B", handle="hb"),
        ]
        self.formatter.print_tasks(tasks, output_format="csv")
        raw = self.output.getvalue()
        lines = raw.strip().split("\n")
        assert len(lines) == 3  # header + 2 rows
        assert "handle" in lines[0]
        assert "Task A" in lines[1]
        assert "Task B" in lines[2]

    def test_csv_empty(self):
        self.formatter.print_tasks([], output_format="csv")
        raw = self.output.getvalue().strip()
        assert raw == ""


class TestPrintTasksTable:
    """Tests for print_tasks with table output."""

    def setup_method(self):
        self.output = StringIO()
        self.console = Console(file=self.output, force_terminal=False)
        self.formatter = TaskFormatter(self.console)

    def test_table_empty(self):
        self.formatter.print_tasks([], output_format="table")
        assert "No tasks found" in self.output.getvalue()

    def test_table_shows_count(self):
        tasks = [_make_task(title=f"T{i}") for i in range(3)]
        self.formatter.print_tasks(tasks, output_format="table")
        assert "3 task(s)" in self.output.getvalue()

    def test_table_shows_limit_count(self):
        tasks = [_make_task(title=f"T{i}") for i in range(10)]
        self.formatter.print_tasks(tasks, output_format="table", limit=5)
        assert "Showing 5 of 10 task(s)" in self.output.getvalue()

    def test_table_has_tags_column(self):
        tasks = [_make_task(tags=["work"])]
        self.formatter.print_tasks(tasks, output_format="table")
        output = self.output.getvalue()
        assert "Tags" in output
        assert "work" in output

    def test_table_sort_by_heading(self):
        tasks = [
            _make_task(title="Zebra"),
            _make_task(title="Apple"),
        ]
        self.formatter.print_tasks(tasks, output_format="json", sort_key="heading")
        data = json.loads(self.output.getvalue())
        assert data[0]["title"] == "Apple"
        assert data[1]["title"] == "Zebra"

    def test_table_sort_by_priority(self):
        tasks = [
            _make_task(title="Low", priority="C"),
            _make_task(title="High", priority="A"),
            _make_task(title="Med", priority=None),
        ]
        self.formatter.print_tasks(tasks, output_format="json", sort_key="priority")
        data = json.loads(self.output.getvalue())
        assert data[0]["title"] == "High"


class TestSortKeys:
    """Tests for SORT_KEYS lambdas."""

    def test_priority_sort_order(self):
        tasks = [
            _make_task(priority="C"),
            _make_task(priority="A"),
            _make_task(priority=None),
        ]
        tasks.sort(key=SORT_KEYS["priority"])
        assert tasks[0].priority == "A"
        assert tasks[1].priority is None  # defaults to "B" = 1
        assert tasks[2].priority == "C"

    def test_due_sort_none_last(self):
        tasks = [
            _make_task(deadline=None),
            _make_task(deadline=date(2026, 1, 1)),
        ]
        tasks.sort(key=SORT_KEYS["due"])
        assert tasks[0].deadline == date(2026, 1, 1)
        assert tasks[1].deadline is None

    def test_heading_sort(self):
        tasks = [
            _make_task(title="Zulu"),
            _make_task(title="alpha"),
        ]
        tasks.sort(key=SORT_KEYS["heading"])
        assert tasks[0].title == "alpha"
        assert tasks[1].title == "Zulu"


class TestStatusColors:
    """Tests for STATUS_COLORS mapping."""

    def test_known_statuses(self):
        assert "TODO" in STATUS_COLORS
        assert "STRT" in STATUS_COLORS
        assert "WAIT" in STATUS_COLORS
        assert "DONE" in STATUS_COLORS

    def test_output_formats_tuple(self):
        assert "table" in OUTPUT_FORMATS
        assert "json" in OUTPUT_FORMATS
        assert "csv" in OUTPUT_FORMATS
