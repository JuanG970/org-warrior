"""Tests for add task functionality."""

import pytest
from unittest.mock import patch, MagicMock
from typer.testing import CliRunner

from org_warrior.main import app
from org_warrior.Org import _escape_elisp

runner = CliRunner()


def _mock_handle_cache(resolve_return="fake-org-id"):
    mock_hc = MagicMock()
    mock_hc.resolve.return_value = resolve_return
    mock_hc.get_handle.return_value = "alpha-bravo-charlie"
    mock_hc.__enter__ = MagicMock(return_value=mock_hc)
    mock_hc.__exit__ = MagicMock(return_value=False)
    return mock_hc


class TestAddWithParent:
    """Test the add command --parent option."""

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value='"new-child-id"')
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_with_parent_creates_child(self, mock_hc_cls, mock_add_child):
        """add --parent delegates to add_child_task."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add", "--parent", "parent-handle", "Subtask title", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Created" in result.output
        mock_add_child.assert_called_once_with("fake-org-id", "Subtask title")

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value="NOT_FOUND")
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_with_parent_not_found(self, mock_hc_cls, mock_add_child):
        """add --parent fails when parent not found."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add", "--parent", "bad-handle", "Subtask", "--no-git"]
        )
        assert result.exit_code != 0

    @patch("org_warrior.Org.OrgQL.add_task", return_value="new-org-id")
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_without_parent_uses_inbox(self, mock_hc_cls, mock_add_task):
        """add without --parent still adds to inbox."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["add", "New inbox task", "--no-git"])
        assert result.exit_code == 0
        assert "Created" in result.output
        mock_add_task.assert_called_once()

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value='"child-id"')
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_short_parent_flag(self, mock_hc_cls, mock_add_child):
        """Short -p flag works the same as --parent."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["add", "-p", "parent-handle", "Subtask", "--no-git"])
        assert result.exit_code == 0
        mock_add_child.assert_called_once_with("fake-org-id", "Subtask")


class TestEscapeElispForAdd:
    """Test elisp escaping for add command."""

    def test_escape_simple_title(self):
        """Test escaping a simple title."""
        title = "Simple task"
        escaped = _escape_elisp(title)
        assert escaped == "Simple task"

    def test_escape_title_with_quotes(self):
        """Test escaping a title with double quotes."""
        title = 'Task with "quotes"'
        escaped = _escape_elisp(title)
        # Quotes should be escaped
        assert '\\"' in escaped

    def test_escape_title_with_backslashes(self):
        """Test escaping a title with backslashes."""
        title = "Task with \\ backslash"
        escaped = _escape_elisp(title)
        # Backslashes should be escaped
        assert "\\\\" in escaped

    def test_escape_title_with_both(self):
        """Test escaping a title with both quotes and backslashes."""
        title = 'Task with "quotes" and \\ backslash'
        escaped = _escape_elisp(title)
        assert '\\"' in escaped
        assert "\\\\" in escaped

    def test_escape_empty_title(self):
        """Test escaping an empty title."""
        title = ""
        escaped = _escape_elisp(title)
        assert escaped == ""
