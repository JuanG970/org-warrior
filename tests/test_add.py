"""Tests for add task functionality."""

import pytest
from org_warrior.Org import _escape_elisp


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
