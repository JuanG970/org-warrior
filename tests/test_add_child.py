"""Tests for the add-child command."""

import pytest
from unittest.mock import patch, MagicMock
from typer.testing import CliRunner

from org_warrior.main import app

runner = CliRunner()


def _mock_handle_cache(resolve_return="fake-org-id"):
    """Create a mock HandleCache that works as context manager."""
    mock_hc = MagicMock()
    mock_hc.resolve.return_value = resolve_return
    mock_hc.get_handle.return_value = "alpha-bravo-charlie"
    mock_hc.__enter__ = MagicMock(return_value=mock_hc)
    mock_hc.__exit__ = MagicMock(return_value=False)
    return mock_hc


class TestAddChildCLI:
    """Test the add-child CLI command."""

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value='"new-child-id"')
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_child_success(self, mock_hc_cls, mock_add_child):
        """Test successfully adding a child task."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add-child", "parent-handle", "Buy groceries", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Created child" in result.output
        assert "alpha-bravo-charlie" in result.output
        mock_add_child.assert_called_once_with("fake-org-id", "Buy groceries")

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value="NOT_FOUND")
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_child_parent_not_found(self, mock_hc_cls, mock_add_child):
        """Error when parent task not found in org files."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add-child", "bad-handle", "Child task", "--no-git"]
        )
        assert result.exit_code != 0

    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_child_handle_not_resolved(self, mock_hc_cls):
        """Error when handle cannot be resolved."""
        mock_hc = _mock_handle_cache(resolve_return=None)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add-child", "unknown-handle", "Child task", "--no-git"]
        )
        assert result.exit_code != 0

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value=None)
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_child_emacs_returns_none(self, mock_hc_cls, mock_add_child):
        """Error when Emacs returns no result."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add-child", "parent-handle", "Child task", "--no-git"]
        )
        assert result.exit_code != 0

    @patch(
        "org_warrior.Org.OrgQL.add_child_task",
        return_value="ERROR: some emacs error",
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_child_emacs_error(self, mock_hc_cls, mock_add_child):
        """Test handling of Emacs error response."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["add-child", "parent-handle", "Child task", "--no-git"]
        )
        assert result.exit_code != 0

    @patch("org_warrior.Org.OrgQL.add_child_task", return_value='"new-child-id"')
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_child_special_chars_in_title(self, mock_hc_cls, mock_add_child):
        """Test child task with special characters in title."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        title = 'Task with "quotes" and \\backslash'
        result = runner.invoke(app, ["add-child", "parent-handle", title, "--no-git"])
        assert result.exit_code == 0
        mock_add_child.assert_called_once_with("fake-org-id", title)

    def test_add_child_empty_title(self):
        """Error when title is empty whitespace."""
        result = runner.invoke(app, ["add-child", "parent-handle", "   ", "--no-git"])
        assert result.exit_code != 0


class TestAddChildTaskMethod:
    """Test OrgQL.add_child_task static method."""

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_org_id(self, mock_run):
        """Returns the new child's org ID on success."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = '"abc-123-def"'
        result = OrgQL.add_child_task("parent-id", "Child title")
        assert result == '"abc-123-def"'
        mock_run.assert_called_once()
        call_args = mock_run.call_args
        assert call_args[0][0] == "add-child-task.el"
        assert call_args[1]["params"]["parent_org_id"] == "parent-id"
        assert call_args[1]["params"]["title"] == "Child title"

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_not_found(self, mock_run):
        """Returns NOT_FOUND when parent doesn't exist."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = "NOT_FOUND"
        result = OrgQL.add_child_task("bad-id", "Child")
        assert result == "NOT_FOUND"

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_none_on_empty(self, mock_run):
        """Returns None when Emacs returns empty."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = None
        result = OrgQL.add_child_task("parent-id", "Child")
        assert result is None

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_none_on_exception(self, mock_run):
        """Returns None when an exception occurs."""
        from org_warrior.Org import OrgQL

        mock_run.side_effect = Exception("connection failed")
        result = OrgQL.add_child_task("parent-id", "Child")
        assert result is None

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_escapes_special_chars(self, mock_run):
        """Verifies special characters are escaped in params."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = '"new-id"'
        OrgQL.add_child_task('parent"id', 'title\\with"quotes')
        call_args = mock_run.call_args
        params = call_args[1]["params"]
        assert params["parent_org_id"] == 'parent\\"id'
        assert params["title"] == 'title\\\\with\\"quotes'
