"""Tests for the note command."""

import pytest
from unittest.mock import patch, MagicMock
from typer.testing import CliRunner

from org_warrior.main import app

runner = CliRunner()


class TestNoteCLI:
    """Test the note CLI command."""

    @patch(
        "org_warrior.Org.OrgQL.add_note",
        return_value=(True, "Note added to: My Task"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_note_success(self, mock_hc_cls, mock_add_note):
        """Test successfully adding a note."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["note", "my-handle", "This is a test note", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Note added" in result.output
        mock_add_note.assert_called_once_with("fake-org-id", "This is a test note")

    @patch(
        "org_warrior.Org.OrgQL.add_note",
        return_value=(False, "Task not found"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_note_task_not_found(self, mock_hc_cls, mock_add_note):
        """Error when task not found."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["note", "bad-handle", "some note", "--no-git"])
        assert result.exit_code != 0

    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_note_handle_not_resolved(self, mock_hc_cls):
        """Error when handle cannot be resolved."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = None
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["note", "unknown-handle", "some note", "--no-git"])
        assert result.exit_code != 0

    @patch(
        "org_warrior.Org.OrgQL.add_note",
        return_value=(True, "Note added to: My Task"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_note_with_special_chars(self, mock_hc_cls, mock_add_note):
        """Test note with special characters."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        note_text = 'Note with "quotes" and \\backslash'
        result = runner.invoke(app, ["note", "my-handle", note_text, "--no-git"])
        assert result.exit_code == 0
        mock_add_note.assert_called_once_with("fake-org-id", note_text)

    @patch(
        "org_warrior.Org.OrgQL.add_note",
        return_value=(False, "ERROR: some emacs error"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_note_emacs_error(self, mock_hc_cls, mock_add_note):
        """Test handling of Emacs error response."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(app, ["note", "my-handle", "some note", "--no-git"])
        assert result.exit_code != 0
