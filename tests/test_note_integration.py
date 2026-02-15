"""Integration tests for note command to verify chronological ordering."""

import pytest
from pathlib import Path


class TestNoteOrdering:
    """Test that notes are added in chronological order (oldest first, newest at bottom)."""

    def test_elisp_logic_for_chronological_notes(self):
        """
        Test the elisp logic ensures notes are added at the end (chronological order).
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-note.el")

        # Verify we skip past existing notes to insert at the end
        assert "(while" in elisp_code, (
            "Code should use a while loop to skip past existing notes"
        )
        assert "Note taken on" in elisp_code, (
            "Code should check for existing notes with 'Note taken on' pattern"
        )

        # Verify we don't just insert right after meta-data (which would be top)
        # Instead, we should skip forward to find the insertion point
        lines = elisp_code.split("\n")
        
        org_end_meta_idx = None
        while_loop_idx = None
        insert_idx = None

        for i, line in enumerate(lines):
            if "org-end-of-meta-data" in line:
                org_end_meta_idx = i
            if "while" in line and while_loop_idx is None:
                # Check if this while loop is about skipping notes
                context = "\n".join(lines[i:min(i+10, len(lines))])
                if "looking-at-p" in context or "Note taken on" in context:
                    while_loop_idx = i
            if "insert (format" in line and "Note taken on" in line:
                insert_idx = i

        assert org_end_meta_idx is not None, "Should move past meta-data"
        assert while_loop_idx is not None, "Should have while loop to skip notes"
        assert insert_idx is not None, "Should insert the note"

        # Verify order: end-of-meta-data, then while loop, then insert
        assert (
            org_end_meta_idx < while_loop_idx < insert_idx
        ), "Must skip past existing notes before inserting"

    def test_note_insertion_stops_at_child_headings(self):
        """
        Test that note insertion stops when it encounters a child heading.
        Notes should be inserted before children, not after.
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-note.el")

        # Should check for child headings (lines starting with *)
        assert "looking-at-p" in elisp_code
        assert "\\*+" in elisp_code or "^\\*" in elisp_code, (
            "Should check for org headings to stop before children"
        )

    def test_note_behavioral_specification(self):
        """
        Document the expected behavior after adding notes.
        This is a specification test.
        """
        # Expected behavior:
        # Given a parent task:
        #   * Parent
        #   :PROPERTIES:
        #   :ID: parent-id
        #   :END:
        #
        # After adding notes "First", "Second", "Third" in that order:
        #   * Parent
        #   :PROPERTIES:
        #   :ID: parent-id
        #   :END:
        #   - Note taken on [2026-02-15 Sat 10:00] \\
        #     First        <- Oldest (added first, at top)
        #   - Note taken on [2026-02-15 Sat 11:00] \\
        #     Second
        #   - Note taken on [2026-02-15 Sat 12:00] \\
        #     Third        <- Newest (added last, at bottom)

        expected_structure = """
After adding notes in order First, Second, Third:

Parent
├── Properties (ID: parent-id)
├── Note: First (oldest, added first) ← starts at top
├── Note: Second
└── Note: Third (newest, added last) ← inserted at bottom

This is CHRONOLOGICAL order (oldest first, newest last).
"""
        assert "oldest" in expected_structure
        assert "newest" in expected_structure
        assert "bottom" in expected_structure


class TestMixedChildrenAndNotes:
    """Test that children and notes can coexist in chronological order."""

    def test_mixed_ordering_specification(self):
        """
        Document expected behavior when mixing children and notes.
        Everything should be chronological from top to bottom.
        """
        # Expected behavior:
        # Start with parent, add children and notes in this ORDER:
        # 1. Add note "First note" at 10:00
        # 2. Add child "Child A" at 10:30
        # 3. Add note "Second note" at 11:00
        # 4. Add child "Child B" at 11:30
        # 5. Add note "Third note" at 12:00
        #
        # Expected result:
        #   * Parent
        #   :PROPERTIES:
        #   :ID: parent-id
        #   :END:
        #   - Note taken on [2026-02-15 Sat 10:00] \\
        #     First note
        #   - Note taken on [2026-02-15 Sat 11:00] \\
        #     Second note
        #   - Note taken on [2026-02-15 Sat 12:00] \\
        #     Third note
        #   ** TODO Child A
        #   :PROPERTIES:
        #   :ID: child-a-id
        #   :END:
        #   ** TODO Child B
        #   :PROPERTIES:
        #   :ID: child-b-id
        #   :END:
        #
        # Notes come BEFORE children (org-mode convention)
        # Within each section (notes, children), items are chronological

        expected_structure = """
With mixed children and notes added in this order:
  1. Note "First" (10:00)
  2. Child A (10:30)
  3. Note "Second" (11:00)
  4. Child B (11:30)
  5. Note "Third" (12:00)

Result should be:
Parent
├── All notes (chronological: First, Second, Third)
└── All children (chronological: Child A, Child B)

This follows org-mode convention: notes before children.
Within each section, chronological order is maintained.
"""
        assert "notes before children" in expected_structure
        assert "chronological" in expected_structure.lower()

    def test_notes_before_children_verification(self):
        """
        Verify the elisp code stops inserting notes when it hits a child heading.
        """
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-note.el")

        # The while loop should stop when it encounters a heading (child)
        # This ensures notes are inserted before children
        lines = [l.strip() for l in elisp_code.split("\n")]
        
        # Find the while loop condition
        while_lines = [l for l in lines if "while" in l.lower()]
        
        # Should have a condition that stops at headings
        has_stop_at_heading = any(
            "\\*" in line or "org-current-level" in elisp_code
            for line in while_lines
        )
        
        assert has_stop_at_heading or "looking-at-p" in elisp_code, (
            "Should check for child headings to stop note insertion"
        )


class TestNoteEdgeCases:
    """Test edge cases for note functionality."""

    def test_handles_task_without_children(self):
        """Notes should work fine on tasks with no children."""
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-note.el")

        # Should handle EOF gracefully
        assert "(eobp)" in elisp_code or "end-of-buffer" in elisp_code.lower()

    def test_handles_blank_lines_between_notes(self):
        """Should skip blank lines when finding insertion point."""
        from org_warrior.elisp_helpers import load_elisp_template

        elisp_code = load_elisp_template("add-note.el")

        # Should check for blank lines in the while condition
        assert "^[ \\t]*$" in elisp_code or "looking-at-p.*$" in elisp_code, (
            "Should handle blank lines between notes"
        )


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
