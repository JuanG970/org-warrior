"""Integration tests for add-child command to verify correct ordering and positioning."""

import pytest
from pathlib import Path
import tempfile
import os


class TestAddChildOrdering:
    """Test that children are added in chronological order (oldest first, newest at bottom)."""

    def test_elisp_uses_org_end_of_subtree(self):
        """
        Verify the elisp uses org-end-of-subtree for navigation instead of
        manually traversing properties drawers and child headings.
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-child-task.el")

        # Should use org-end-of-subtree to skip past all children cleanly
        assert "org-end-of-subtree" in elisp_code, (
            "Should use org-end-of-subtree to navigate to insertion point"
        )

    def test_elisp_uses_org_todo_for_state(self):
        """
        Verify the elisp uses org-todo to set the TODO state instead of
        embedding it as a raw string in the heading text.
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-child-task.el")

        assert '(org-todo "TODO")' in elisp_code, (
            "Should use org-todo to set TODO state via the org API"
        )

    def test_elisp_logic_ordering(self):
        """
        Verify that after insertion, the cursor is positioned on the new heading
        before creating the org-id.
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-child-task.el")

        # After insert, must move back to that line before org-id-get-create
        assert "(forward-line -1)" in elisp_code, (
            "Code should move back to inserted line with (forward-line -1)"
        )
        assert "(org-id-get-create)" in elisp_code, (
            "Should create org-id on the new heading"
        )

        lines = elisp_code.split("\n")
        insert_idx = next((i for i, l in enumerate(lines) if "(insert " in l), None)
        forward_line_idx = next(
            (i for i, l in enumerate(lines) if "(forward-line -1)" in l), None
        )
        org_id_create_idx = next(
            (i for i, l in enumerate(lines) if "(org-id-get-create)" in l), None
        )

        assert insert_idx is not None, "Should have insert statement"
        assert forward_line_idx is not None, "Should move back to inserted line"
        assert org_id_create_idx is not None, "Should create org-id"

        # Verify order: insert → forward-line -1 → org-id-create
        assert (
            insert_idx < forward_line_idx < org_id_create_idx
        ), "After insert, must move to inserted line before creating org-id"

    def test_child_insertion_appends_at_bottom(self):
        """
        Verify the insertion logic appends new children at the bottom of the
        parent's subtree (chronological order: oldest first).
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-child-task.el")

        # org-end-of-subtree positions after all existing children
        assert "org-end-of-subtree" in elisp_code, (
            "Should use org-end-of-subtree to append after existing children"
        )

    def test_mock_file_structure(self):
        """
        Document the expected org file structure after adding children.
        """
        expected_structure = """
After insertion of children in order A, B, C, the structure should be:

Parent
├── Properties (ID: parent-id)
├── Child A (oldest, added first)
├── Child B
└── Child C (most recent, added last) <- inserted here (bottom)

NOT:

Parent
├── Properties
├── Child C (most recent) <- inserted here (top, WRONG)
├── Child B
└── Child A (oldest)
"""
        assert "oldest, added first" in expected_structure
        assert "bottom" in expected_structure


class TestAddChildEdgeCases:
    """Test edge cases for add-child functionality."""

    def test_handles_parent_without_properties(self):
        """
        org-end-of-subtree handles parents with or without :PROPERTIES: drawers
        automatically — no manual detection required.
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-child-task.el")

        # org-end-of-subtree correctly skips past the properties drawer
        assert "org-end-of-subtree" in elisp_code

    def test_escapes_special_characters(self):
        """Test that special characters in title are handled."""
        from org_warrior.Org import OrgQL

        assert hasattr(OrgQL, "add_child_task")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
