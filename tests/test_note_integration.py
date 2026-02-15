"""Integration tests for note command — ordering and formatting.

These tests verify:
  1. Notes are inserted in chronological order (newest at bottom).
  2. Each note is a separate bullet with correct formatting.
  3. The elisp template structure matches expectations.

Tests marked ``@pytest.mark.emacs`` require a running Emacs daemon and
are skipped automatically when one is not available.
"""

import os
import re
import time
import shutil
import tempfile
import subprocess
import textwrap
import uuid

import pytest

from org_warrior.elisp_helpers import load_elisp_template

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

NOTE_RE = re.compile(
    r"^- Note taken on \[(\d{4}-\d{2}-\d{2} \w{2,3} \d{2}:\d{2})\]\n  (.+)$",
    re.MULTILINE,
)
"""Regex that matches a well-formed note bullet.

Group 1 = timestamp, Group 2 = note body text.
Note: No backslash at end of timestamp line - clean formatting.
"""


def _emacs_available() -> bool:
    """Return True if an Emacs daemon named 'edit' (or configured) is reachable."""
    from org_warrior import config

    try:
        result = subprocess.run(
            ["emacsclient", "-s", config.EMACS_SERVER, "-e", "(+ 1 1)"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        return result.returncode == 0 and result.stdout.strip() == "2"
    except Exception:
        return False


emacs = pytest.mark.skipif(not _emacs_available(), reason="Emacs daemon not running")


def _add_note_via_emacs(org_id: str, note_text: str) -> str:
    """Call add_note through the real Python -> elisp pipeline and return the message."""
    from org_warrior.Org import OrgQL

    ok, msg = OrgQL.add_note(org_id, note_text)
    assert ok, f"add_note failed: {msg}"
    return msg


def _create_temp_org_file(content: str) -> str:
    """Write *content* to a temp .org file and return its path."""
    fd, path = tempfile.mkstemp(suffix=".org")
    os.write(fd, content.encode())
    os.close(fd)
    return path


# ---------------------------------------------------------------------------
# Elisp template structure tests (no Emacs needed)
# ---------------------------------------------------------------------------


class TestElispTemplateStructure:
    """Verify the elisp template has the expected shape."""

    def test_inserts_after_existing_notes(self):
        """After org-end-of-meta-data, the code should skip existing notes to append."""
        elisp = load_elisp_template("add-note.el")
        lines = elisp.split("\n")

        meta_idx = None
        insert_idx = None
        for i, line in enumerate(lines):
            if "org-end-of-meta-data" in line:
                meta_idx = i
            if "insert (format" in line and "Note taken on" in line:
                insert_idx = i

        assert meta_idx is not None, "Should call org-end-of-meta-data"
        assert insert_idx is not None, "Should have an insert statement"
        assert insert_idx > meta_idx, "insert must come after org-end-of-meta-data"

        # There SHOULD be a while loop between meta-data and insert to skip past notes
        between = "\n".join(lines[meta_idx:insert_idx])
        assert "(while" in between, (
            "Should skip past existing notes to append at the bottom for "
            "chronological order"
        )

    def test_format_string_has_no_backslash(self):
        r"""The format string should NOT have a backslash before the newline."""
        elisp = load_elisp_template("add-note.el")
        # The format should be clean: "- Note taken on [%s]\n  %s\n"
        # No backslash continuation needed
        assert "\\\\\\n" not in elisp, (
            r"Format string should NOT contain \\\n (no backslash continuation)"
        )

    def test_note_format_has_correct_structure(self):
        """The insert format should produce '- Note taken on [TS] \\\\n  TEXT\\n'."""
        elisp = load_elisp_template("add-note.el")
        assert "- Note taken on [%s]" in elisp
        assert "{note}" in elisp


# ---------------------------------------------------------------------------
# Real Emacs integration tests
# ---------------------------------------------------------------------------


@emacs
class TestNoteIntegration:
    """End-to-end tests that add notes via the real Emacs daemon."""

    def _make_task_file(self, org_id: str | None = None) -> tuple[str, str]:
        """Create a minimal org file with one task and return (path, org_id).

        The file is registered with org-id so Emacs can find it.
        """
        from org_warrior import config

        org_id = org_id or str(uuid.uuid4())
        content = textwrap.dedent(f"""\
            * TODO Test task for notes
            :PROPERTIES:
            :ID: {org_id}
            :END:
        """)
        path = _create_temp_org_file(content)

        # Tell Emacs to visit this file so org-id can resolve the ID.
        subprocess.run(
            [
                "emacsclient",
                "-s",
                config.EMACS_SERVER,
                "-e",
                f'(progn (find-file-noselect "{path}") (org-id-update-id-locations (list "{path}")))',
            ],
            capture_output=True,
            text=True,
            timeout=30,
        )
        return path, org_id

    # ---- ordering ---------------------------------------------------------

    def test_newest_note_appears_last(self):
        """After adding Note A then Note B, Note A should be above Note B (chronological)."""
        path, org_id = self._make_task_file()
        try:
            _add_note_via_emacs(org_id, "Note A")
            time.sleep(1)  # ensure distinct timestamps when possible
            _add_note_via_emacs(org_id, "Note B")

            text = open(path).read()
            matches = NOTE_RE.findall(text)
            assert len(matches) == 2, (
                f"Expected 2 notes, found {len(matches)}.\nFile contents:\n{text}"
            )
            # matches is [(ts, body), ...] in file order (top to bottom)
            assert matches[0][1] == "Note A", "Oldest note (A) should be first (top)"
            assert matches[1][1] == "Note B", (
                "Newest note (B) should be second (bottom)"
            )
        finally:
            os.unlink(path)

    def test_three_notes_chronological(self):
        """Adding A, B, C should produce A, B, C from top to bottom."""
        path, org_id = self._make_task_file()
        try:
            for note in ("Note A", "Note B", "Note C"):
                _add_note_via_emacs(org_id, note)

            text = open(path).read()
            matches = NOTE_RE.findall(text)
            assert len(matches) == 3, f"Expected 3 notes.\nFile:\n{text}"
            bodies = [m[1] for m in matches]
            assert bodies == ["Note A", "Note B", "Note C"], (
                f"Notes should be oldest-first (chronological), got: {bodies}"
            )
        finally:
            os.unlink(path)

    # ---- formatting -------------------------------------------------------

    def test_each_note_is_separate_bullet(self):
        """Each note must be its own '- Note taken on ...' bullet."""
        path, org_id = self._make_task_file()
        try:
            _add_note_via_emacs(org_id, "Alpha")
            _add_note_via_emacs(org_id, "Beta")

            text = open(path).read()
            bullets = re.findall(r"^- Note taken on \[", text, re.MULTILINE)
            assert len(bullets) == 2, (
                f"Expected 2 separate note bullets, found {len(bullets)}.\n"
                f"File:\n{text}"
            )
        finally:
            os.unlink(path)

    def test_note_format_no_backslash(self):
        r"""Each note line should NOT end with a backslash - clean formatting."""
        path, org_id = self._make_task_file()
        try:
            _add_note_via_emacs(org_id, "Check formatting")

            text = open(path).read()
            # Find the note timestamp line
            ts_lines = [
                l
                for l in text.splitlines()
                if l.strip().startswith("- Note taken on [")
            ]
            assert len(ts_lines) == 1

            # Should end with closing bracket ']', NOT a backslash
            line = ts_lines[0].rstrip()
            assert line.endswith("]"), (
                f"Timestamp line should end with ']', got: {line!r}"
            )
            assert not line.endswith("\\"), (
                f"Timestamp line should NOT end with backslash, got: {line!r}"
            )
        finally:
            os.unlink(path)

    def test_note_body_indented_on_next_line(self):
        """The note body text should be on the line after the timestamp, indented."""
        path, org_id = self._make_task_file()
        try:
            _add_note_via_emacs(org_id, "My note body")

            lines = open(path).read().splitlines()
            for i, line in enumerate(lines):
                if "- Note taken on [" in line:
                    assert i + 1 < len(lines), "Body line missing after timestamp"
                    body_line = lines[i + 1]
                    assert body_line.strip() == "My note body", (
                        f"Body should be 'My note body', got: {body_line!r}"
                    )
                    assert body_line.startswith("  "), (
                        f"Body should be indented with 2 spaces, got: {body_line!r}"
                    )
                    break
            else:
                pytest.fail("No note timestamp line found in file")
        finally:
            os.unlink(path)

    # ---- coexistence with children ----------------------------------------

    def test_notes_before_child_headings(self):
        """Notes should appear between the property drawer and any child headings."""
        from org_warrior import config

        org_id = str(uuid.uuid4())
        child_id = str(uuid.uuid4())
        content = textwrap.dedent(f"""\
            * TODO Parent task
            :PROPERTIES:
            :ID: {org_id}
            :END:
            ** TODO Existing child
            :PROPERTIES:
            :ID: {child_id}
            :END:
        """)
        path = _create_temp_org_file(content)

        subprocess.run(
            [
                "emacsclient",
                "-s",
                config.EMACS_SERVER,
                "-e",
                f'(progn (find-file-noselect "{path}") (org-id-update-id-locations (list "{path}")))',
            ],
            capture_output=True,
            text=True,
            timeout=30,
        )

        try:
            _add_note_via_emacs(org_id, "Note on parent")

            text = open(path).read()
            note_pos = text.find("- Note taken on [")
            child_pos = text.find("** TODO Existing child")
            assert note_pos != -1, f"Note not found.\nFile:\n{text}"
            assert child_pos != -1, f"Child heading not found.\nFile:\n{text}"
            assert note_pos < child_pos, (
                "Note should appear BEFORE child heading.\n"
                f"Note at {note_pos}, child at {child_pos}.\nFile:\n{text}"
            )
        finally:
            os.unlink(path)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
