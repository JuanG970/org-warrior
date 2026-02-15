"""Tests for modify tag and property functionality."""

import pytest
from unittest.mock import patch, MagicMock
from typer.testing import CliRunner

from org_warrior.Org import _escape_elisp, OrgQL
from org_warrior.main import app


runner = CliRunner()


class TestTagEscaping:
    """Test tag-related elisp escaping."""

    def test_tag_with_special_chars(self):
        """Tags with special characters get escaped for elisp."""
        tag = 'tag"with"quotes'
        assert _escape_elisp(tag) == 'tag\\"with\\"quotes'

    def test_tag_with_backslash(self):
        """Tags with backslash get escaped for elisp."""
        tag = "path\\tag"
        assert _escape_elisp(tag) == "path\\\\tag"

    def test_simple_tag(self):
        """Simple tag passes through unchanged."""
        assert _escape_elisp("work") == "work"

    def test_empty_tag(self):
        """Empty string passes through."""
        assert _escape_elisp("") == ""


class TestPropertyValidation:
    """Test property key/value validation."""

    def test_simple_property_key(self):
        """Simple property key passes through."""
        assert _escape_elisp("EFFORT") == "EFFORT"

    def test_property_key_with_special_chars(self):
        """Property key with special chars is escaped."""
        assert _escape_elisp('key"val') == 'key\\"val'

    def test_simple_property_value(self):
        """Simple property value passes through."""
        assert _escape_elisp("30min") == "30min"

    def test_property_value_with_quotes(self):
        """Property value with quotes is escaped."""
        assert _escape_elisp('say "hello"') == 'say \\"hello\\"'

    def test_empty_property_value(self):
        """Empty value is valid (used for remove action)."""
        assert _escape_elisp("") == ""


class TestModifyTagAddCLI:
    """Test the modify tag add CLI subcommand."""

    @patch(
        "org_warrior.Org.OrgQL.modify_tags",
        return_value=(True, "Added work on My Task"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_tag(self, mock_hc_cls, mock_modify):
        """Test adding a tag via CLI."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["modify", "tag", "add", "my-handle", "work", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Tag added" in result.output
        mock_modify.assert_called_once_with("fake-org-id", "add", "work")

    @patch("org_warrior.Org.OrgQL.modify_tags", return_value=(False, "Task not found"))
    @patch("org_warrior.HandleCache.HandleCache")
    def test_add_tag_task_not_found(self, mock_hc_cls, mock_modify):
        """Error when task not found."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["modify", "tag", "add", "bad-handle", "work", "--no-git"]
        )
        assert result.exit_code != 0


class TestModifyTagRemoveCLI:
    """Test the modify tag remove CLI subcommand."""

    @patch(
        "org_warrior.Org.OrgQL.modify_tags",
        return_value=(True, "Removed old on My Task"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_remove_tag(self, mock_hc_cls, mock_modify):
        """Test removing a tag via CLI."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["modify", "tag", "remove", "my-handle", "old", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Tag removed" in result.output
        mock_modify.assert_called_once_with("fake-org-id", "remove", "old")

    @patch("org_warrior.Org.OrgQL.modify_tags", return_value=(False, "Task not found"))
    @patch("org_warrior.HandleCache.HandleCache")
    def test_remove_tag_task_not_found(self, mock_hc_cls, mock_modify):
        """Error when task not found."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["modify", "tag", "remove", "bad-handle", "old", "--no-git"]
        )
        assert result.exit_code != 0


class TestModifyPropertyCLI:
    """Test the modify property CLI subcommand."""

    @patch(
        "org_warrior.Org.OrgQL.modify_property",
        return_value=(True, "Set EFFORT=30min on My Task"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_set_property(self, mock_hc_cls, mock_modify):
        """Test setting a property via CLI."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["modify", "property", "my-handle", "EFFORT", "30min", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Property modified" in result.output
        mock_modify.assert_called_once_with("fake-org-id", "set", "EFFORT", "30min")

    @patch(
        "org_warrior.Org.OrgQL.modify_property",
        return_value=(True, "Removed EFFORT on My Task"),
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_remove_property(self, mock_hc_cls, mock_modify):
        """Test removing a property via CLI."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app, ["modify", "property", "my-handle", "EFFORT", "--remove", "--no-git"]
        )
        assert result.exit_code == 0
        assert "Property modified" in result.output
        mock_modify.assert_called_once_with("fake-org-id", "remove", "EFFORT", "")

    def test_property_no_value_no_remove_fails(self):
        """Property without value or --remove flag fails."""
        result = runner.invoke(
            app, ["modify", "property", "my-handle", "EFFORT", "--no-git"]
        )
        assert result.exit_code != 0

    @patch(
        "org_warrior.Org.OrgQL.modify_property", return_value=(False, "Task not found")
    )
    @patch("org_warrior.HandleCache.HandleCache")
    def test_property_task_not_found(self, mock_hc_cls, mock_modify):
        """Error when task not found."""
        mock_hc = MagicMock()
        mock_hc.resolve.return_value = "fake-org-id"
        mock_hc.__enter__ = MagicMock(return_value=mock_hc)
        mock_hc.__exit__ = MagicMock(return_value=False)
        mock_hc_cls.return_value = mock_hc

        result = runner.invoke(
            app,
            ["modify", "property", "bad-handle", "EFFORT", "30min", "--no-git"],
        )
        assert result.exit_code != 0
