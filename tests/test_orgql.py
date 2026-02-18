"""Tests for OrgQL class and related functionality"""

from datetime import datetime
from org_warrior.Org import OrgQL, Task, _escape_elisp, parse_org_ql_result


class TestParseOrgQlResult:
    """Tests for parse_org_ql_result function."""

    def test_parse_org_ql_result_none(self):
        """Test that parse_org_ql_result handles None gracefully."""
        result = parse_org_ql_result(None)
        assert result == []

    def test_parse_org_ql_result_empty_string(self):
        """Test that parse_org_ql_result handles empty string gracefully."""
        result = parse_org_ql_result("")
        assert result == []


class TestEscapeElisp:
    """Tests for _escape_elisp function."""

    def test_escape_backslash(self):
        """Test escaping backslashes."""
        assert _escape_elisp("path\\to\\file") == "path\\\\to\\\\file"

    def test_escape_quotes(self):
        """Test escaping double quotes."""
        assert _escape_elisp('say "hello"') == 'say \\"hello\\"'

    def test_escape_both(self):
        """Test escaping both backslashes and quotes."""
        assert _escape_elisp('path\\to\\"file"') == 'path\\\\to\\\\\\"file\\"'

    def test_no_escape_needed(self):
        """Test string with no special characters."""
        assert _escape_elisp("simple string") == "simple string"

    def test_empty_string(self):
        """Test empty string."""
        assert _escape_elisp("") == ""


class TestOrgQL:
    """Tests for OrgQL class."""

    def test_build_query_empty(self):
        """Test building query with empty string."""
        assert OrgQL.build_query("") == "(and (todo) (not (done)))"

    def test_build_query_whitespace_only(self):
        """Test building query with whitespace only."""
        assert OrgQL.build_query("   ") == "(and (todo) (not (done)))"

    def test_build_query_single_tag(self):
        """Test query with single tag."""
        assert OrgQL.build_query("+work") == '(tags "work")'

    def test_build_query_multiple_tags(self):
        """Test query with multiple tags."""
        assert (
            OrgQL.build_query("+work +urgent") == '(and (tags "work") (tags "urgent"))'
        )

    def test_build_query_project(self):
        """Test query with project filter."""
        assert OrgQL.build_query("project:any") == '(ancestors (tags "project"))'

    def test_build_query_priority_short(self):
        """Test query with priority using short form."""
        assert OrgQL.build_query("pri:A") == '(priority "A")'

    def test_build_query_priority_long(self):
        """Test query with priority using long form."""
        assert OrgQL.build_query("priority:B") == '(priority "B")'

    def test_build_query_priority_lowercase(self):
        """Test priority conversion to uppercase."""
        assert OrgQL.build_query("pri:c") == '(priority "C")'

    def test_build_query_due_today(self):
        """Test query with due:today."""
        assert OrgQL.build_query("due:today") == "(deadline :on today)"

    def test_build_query_due_week(self):
        """Test query with due:week."""
        assert OrgQL.build_query("due:week") == "(deadline :from today :to +7)"

    def test_build_query_due_overdue(self):
        """Test query with due:overdue."""
        assert OrgQL.build_query("due:overdue") == "(deadline :to -1)"

    def test_build_query_scheduled_today(self):
        """Test query with scheduled:today."""
        assert OrgQL.build_query("scheduled:today") == "(scheduled :on today)"

    def test_build_query_regexp(self):
        """Test query with plain text (regexp search)."""
        assert OrgQL.build_query("meeting") == '(regexp "meeting")'

    def test_build_query_complex(self):
        """Test complex query with multiple conditions."""
        result = OrgQL.build_query("+work pri:A due:today meeting")
        assert result.startswith("(and ")
        assert '(tags "work")' in result
        assert '(priority "A")' in result
        assert "(deadline :on today)" in result
        assert '(regexp "meeting")' in result

    def test_build_query_escapes_special_chars(self):
        """Test that special characters in search terms are escaped."""
        result = OrgQL.build_query('+tag"with"quotes')
        assert result == '(tags "tag\\"with\\"quotes")'

    def test_build_query_tag_with_backslash(self):
        """Test tag with backslash is escaped."""
        result = OrgQL.build_query("+path\\to\\tag")
        assert result == '(tags "path\\\\to\\\\tag")'

    def test_build_query_state(self):
        """Test query with state filter."""
        assert OrgQL.build_query("state:STRT") == '(todo "STRT")'

    def test_build_query_state_lowercase(self):
        """Test state filter converts to uppercase."""
        assert OrgQL.build_query("state:wait") == '(todo "WAIT")'

    def test_build_query_status_alias(self):
        """Test status: works as alias for state:."""
        assert OrgQL.build_query("status:TODO") == '(todo "TODO")'

    def test_build_query_state_with_other_filters(self):
        """Test state filter combined with other filters."""
        result = OrgQL.build_query("state:STRT +work")
        assert result.startswith("(and ")
        assert '(todo "STRT")' in result
        assert '(tags "work")' in result

    def test_build_query_or_two_states(self):
        """Test OR between two state filters."""
        result = OrgQL.build_query("state:AGENT or state:STRT")
        assert result == '(or (todo "AGENT") (todo "STRT"))'

    def test_build_query_or_multiple_states(self):
        """Test OR across many state filters."""
        result = OrgQL.build_query(
            "state:AGENT or state:STRT or state:CODE or state:REVIEW or state:BLOCKED"
        )
        assert result.startswith("(or ")
        assert '(todo "AGENT")' in result
        assert '(todo "STRT")' in result
        assert '(todo "CODE")' in result
        assert '(todo "REVIEW")' in result
        assert '(todo "BLOCKED")' in result
        # Should NOT contain regexp "or"
        assert '(regexp "or")' not in result

    def test_build_query_or_with_and_groups(self):
        """Test OR between AND groups."""
        result = OrgQL.build_query("state:CODE +urgent or state:REVIEW +urgent")
        assert result.startswith("(or ")
        assert '(and (todo "CODE") (tags "urgent"))' in result
        assert '(and (todo "REVIEW") (tags "urgent"))' in result

    def test_build_query_explicit_and(self):
        """Test explicit 'and' keyword is accepted (no-op)."""
        result = OrgQL.build_query("state:TODO and +work")
        assert result.startswith("(and ")
        assert '(todo "TODO")' in result
        assert '(tags "work")' in result

    def test_build_query_or_case_insensitive(self):
        """Test OR keyword is case-insensitive."""
        result = OrgQL.build_query("state:AGENT OR state:STRT")
        assert result == '(or (todo "AGENT") (todo "STRT"))'

    def test_build_query_and_case_insensitive(self):
        """Test AND keyword is case-insensitive."""
        result = OrgQL.build_query("state:TODO AND +work")
        assert '(todo "TODO")' in result
        assert '(tags "work")' in result

    def test_build_query_only_or_tokens(self):
        """Test query with only 'or' tokens falls back to default."""
        result = OrgQL.build_query("or or")
        assert result == "(and (todo) (not (done)))"

    def test_build_query_single_after_or(self):
        """Test single condition after or."""
        result = OrgQL.build_query("state:TODO or state:STRT")
        assert result == '(or (todo "TODO") (todo "STRT"))'


class TestTask:
    """Tests for Task dataclass."""

    def test_task_creation_minimal(self):
        """Test creating task with minimal required fields."""
        task = Task(
            org_id="abc-123", title="Test Task", tags=[], location="~/org/todo.org:42"
        )
        assert task.org_id == "abc-123"
        assert task.title == "Test Task"
        assert task.tags == []
        assert task.location == "~/org/todo.org:42"
        assert task.status == "TODO"
        assert task.body is None
        assert task.priority is None
        assert task.scheduled is None
        assert task.deadline is None

    def test_task_creation_full(self):
        """Test creating task with all fields."""
        scheduled = datetime(2026, 2, 15, 10, 0)
        deadline = datetime(2026, 2, 20, 17, 0)

        task = Task(
            org_id="def-456",
            title="Complete Task",
            tags=["work", "urgent"],
            location="~/org/work.org:100",
            status="NEXT",
            body="Task description here",
            priority="A",
            scheduled=scheduled,
            deadline=deadline,
        )

        assert task.org_id == "def-456"
        assert task.title == "Complete Task"
        assert task.tags == ["work", "urgent"]
        assert task.location == "~/org/work.org:100"
        assert task.status == "NEXT"
        assert task.body == "Task description here"
        assert task.priority == "A"
        assert task.scheduled == scheduled
        assert task.deadline == deadline

    def test_is_done_true(self):
        """Test is_done returns True for DONE status."""
        task = Task(
            org_id="done-123",
            title="Completed",
            tags=[],
            location="~/org/done.org:1",
            status="DONE",
        )
        assert task.is_done() is True

    def test_is_done_false_todo(self):
        """Test is_done returns False for TODO status."""
        task = Task(
            org_id="todo-123",
            title="Not Done",
            tags=[],
            location="~/org/todo.org:1",
            status="TODO",
        )
        assert task.is_done() is False

    def test_is_done_false_other_status(self):
        """Test is_done returns False for other statuses."""
        task = Task(
            org_id="next-123",
            title="In Progress",
            tags=[],
            location="~/org/todo.org:1",
            status="NEXT",
        )
        assert task.is_done() is False

    def test_task_with_empty_tags(self):
        """Test task with empty tags list."""
        task = Task(
            org_id="no-tags",
            title="Untagged Task",
            tags=[],
            location="~/org/todo.org:50",
        )
        assert task.tags == []

    def test_task_with_multiple_tags(self):
        """Test task with multiple tags."""
        task = Task(
            org_id="multi-tag",
            title="Tagged Task",
            tags=["personal", "shopping", "errands"],
            location="~/org/personal.org:25",
        )
        assert len(task.tags) == 3
        assert "personal" in task.tags
        assert "shopping" in task.tags
        assert "errands" in task.tags
