"""Tests for TaskFormatter module."""

import pytest
from datetime import datetime
from org_warrior.Org import Task
from org_warrior.TaskFormatter import TaskFormatter
from rich.console import Console
from io import StringIO


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
        task = Task(
            title="Test Task",
            status="TODO",
            org_id="test-id-001",
            location="/path/to/file.org:10",
            tags=[],
        )
        result = self.formatter.format_task_detail(task)

        assert "Task:" in result
        assert "Test Task" in result
        assert "TODO" in result
        assert "test-id-001" in result
        assert "/path/to/file.org:10" in result

    def test_format_task_detail_with_priority(self):
        """Test formatting a task with priority."""
        task = Task(
            title="High Priority Task",
            status="TODO",
            priority="A",
            org_id="test-id-002",
            location="/path/to/file.org:20",
            tags=[],
        )
        result = self.formatter.format_task_detail(task)

        assert "Priority: " in result
        assert "(A)" in result

    def test_format_task_detail_with_dates(self):
        """Test formatting a task with deadline and scheduled dates."""
        task = Task(
            title="Task with Dates",
            status="TODO",
            deadline=datetime(2026, 2, 20, 10, 30),
            scheduled=datetime(2026, 2, 15, 9, 0),
            org_id="test-id-003",
            location="/path/to/file.org:30",
            tags=[],
        )
        result = self.formatter.format_task_detail(task)

        assert "Deadline:" in result
        assert "Scheduled:" in result
        assert "2026-02-" in result  # Check year-month part

    def test_format_task_detail_with_index(self):
        """Test formatting a task with an index number."""
        task = Task(
            title="Indexed Task",
            status="TODO",
            org_id="test-id-004",
            location="/path/to/file.org:40",
            tags=[],
        )
        result = self.formatter.format_task_detail(task, idx=5)

        assert "Task 5:" in result

    def test_format_task_detail_no_status(self):
        """Test formatting a task without a status."""
        task = Task(
            title="No Status Task",
            org_id="test-id-005",
            location="/path/to/file.org:50",
            tags=[],
        )
        result = self.formatter.format_task_detail(task)

        assert "No Status Task" in result
        assert "Task:" in result
