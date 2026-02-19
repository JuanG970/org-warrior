"""Tests for get-parent command and OrgQL.get_parent_task."""

import pytest
from unittest.mock import patch, MagicMock
from typer.testing import CliRunner

from org_warrior.main import app
from org_warrior.Org import Task

runner = CliRunner()


def _mock_handle_cache(resolve_return="fake-org-id"):
    """Create a mock HandleCache that works as context manager."""
    mock_hc = MagicMock()
    mock_hc.resolve.return_value = resolve_return
    mock_hc.get_handle.return_value = "alpha-bravo-charlie"
    mock_hc.__enter__ = MagicMock(return_value=mock_hc)
    mock_hc.__exit__ = MagicMock(return_value=False)
    return mock_hc


def _make_task(**kwargs):
    defaults = dict(
        org_id="parent-id",
        title="Parent Task",
        tags=[],
        location="test.org:1",
        status="TODO",
    )
    defaults.update(kwargs)
    return Task(**defaults)


class TestGetParentCLI:
    """Test the get-parent CLI command."""

    @patch("org_warrior.Org.OrgQL.get_parent_task")
    @patch("org_warrior.HandleCache.HandleCache")
    def test_get_parent_success(self, mock_hc_cls, mock_get_parent):
        """Successfully shows the parent task."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc
        mock_get_parent.return_value = _make_task()

        result = runner.invoke(app, ["get-parent", "child-handle"])
        assert result.exit_code == 0
        mock_get_parent.assert_called_once_with("fake-org-id")

    @patch("org_warrior.Org.OrgQL.get_parent_task", return_value=None)
    @patch("org_warrior.HandleCache.HandleCache")
    def test_get_parent_no_parent(self, mock_hc_cls, mock_get_parent):
        """Returns non-zero when task has no parent."""
        mock_hc = _mock_handle_cache()
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["get-parent", "top-level-handle"])
        assert result.exit_code != 0

    @patch("org_warrior.HandleCache.HandleCache")
    def test_get_parent_handle_not_resolved(self, mock_hc_cls):
        """Returns non-zero when handle cannot be resolved."""
        mock_hc = _mock_handle_cache(resolve_return=None)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["get-parent", "unknown-handle"])
        assert result.exit_code != 0


class TestGetParentTaskMethod:
    """Test OrgQL.get_parent_task static method."""

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_task_with_parent_id(self, mock_run):
        """Returns a Task when parent has an org-id."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = '{"heading":"Parent","id":"parent-id","todo":"TODO","priority":null,"deadline":null,"scheduled":null,"filename":"test.org","linenumber":1,"tags":[],"properties":{}}'
        result = OrgQL.get_parent_task("child-id")
        assert result is not None
        assert result.org_id == "parent-id"
        assert result.title == "Parent"
        mock_run.assert_called_once()
        assert mock_run.call_args[0][0] == "get-parent.el"

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_task_without_id(self, mock_run):
        """Returns a Task even when parent has no org-id (plain heading)."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = '{"heading":"Section","id":null,"todo":null,"priority":null,"deadline":null,"scheduled":null,"filename":"test.org","linenumber":5,"tags":[],"properties":{}}'
        result = OrgQL.get_parent_task("child-id")
        assert result is not None
        assert result.org_id == ""
        assert result.title == "Section"

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_none_for_not_found(self, mock_run):
        """Returns None when task ID does not exist."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = "NOT_FOUND"
        result = OrgQL.get_parent_task("bad-id")
        assert result is None

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_none_for_no_parent(self, mock_run):
        """Returns None when task is at the top level."""
        from org_warrior.Org import OrgQL

        mock_run.return_value = "NO_PARENT"
        result = OrgQL.get_parent_task("top-level-id")
        assert result is None

    @patch("org_warrior.elisp_helpers.emacs_run_elisp_file")
    def test_returns_none_on_exception(self, mock_run):
        """Returns None when connection fails."""
        from org_warrior.Org import OrgQL

        mock_run.side_effect = Exception("connection failed")
        result = OrgQL.get_parent_task("some-id")
        assert result is None


class TestGetParentElispFile:
    """Test that get-parent.el exists and uses the right org functions."""

    def test_elisp_file_exists(self):
        from org_warrior.elisp_helpers import load_elisp_template

        code = load_elisp_template("get-parent.el")
        assert len(code) > 0

    def test_uses_org_up_heading_safe(self):
        from org_warrior.elisp_helpers import load_elisp_template

        code = load_elisp_template("get-parent.el")
        assert "org-up-heading-safe" in code, (
            "Should use org-up-heading-safe to navigate to parent"
        )

    def test_returns_no_parent_sentinel(self):
        from org_warrior.elisp_helpers import load_elisp_template

        code = load_elisp_template("get-parent.el")
        assert "NO_PARENT" in code
        assert "NOT_FOUND" in code
