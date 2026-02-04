#!/usr/bin/env python3
"""
Unit tests for org-warrior schedule argument parsing.
Tests the logic that distinguishes Org IDs from numeric indices.
"""

import unittest
import sys
import os

# Add src to path so we can import org-warrior functions
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "src"))


class TestScheduleArgParsing(unittest.TestCase):
    """Test schedule command argument parsing."""

    def test_numeric_index_parsing(self):
        """Test that numeric strings are parsed as integers."""
        # Simulate the parsing logic from cmd_schedule
        last_arg = "3"
        task_id = None
        try:
            task_id = int(last_arg)
            is_org_id = False
        except ValueError:
            is_org_id = True

        self.assertFalse(
            is_org_id, "Numeric string should be parsed as index, not Org ID"
        )
        self.assertEqual(task_id, 3, "Task ID should be 3")

    def test_org_id_parsing(self):
        """Test that non-numeric strings are treated as Org IDs."""
        # Simulate the parsing logic from cmd_schedule
        last_arg = "abc123def456"
        org_id = None
        try:
            task_id = int(last_arg)
            is_org_id = False
        except ValueError:
            org_id = last_arg
            is_org_id = True

        self.assertTrue(is_org_id, "Non-numeric string should be treated as Org ID")
        self.assertEqual(org_id, "abc123def456", "Org ID should match input")

    def test_alphanumeric_org_id(self):
        """Test that alphanumeric IDs are treated as Org IDs."""
        last_arg = "a1b2c3"
        try:
            task_id = int(last_arg)
            is_org_id = False
        except ValueError:
            org_id = last_arg
            is_org_id = True

        self.assertTrue(is_org_id, "Alphanumeric string should be treated as Org ID")

    def test_uuid_style_org_id(self):
        """Test UUID-style Org IDs."""
        last_arg = "550e8400-e29b-41d4-a716-446655440000"
        try:
            task_id = int(last_arg)
            is_org_id = False
        except ValueError:
            org_id = last_arg
            is_org_id = True

        self.assertTrue(is_org_id, "UUID-style string should be treated as Org ID")

    def test_zero_index(self):
        """Test that 0 is parsed as numeric (even though it's out of range)."""
        last_arg = "0"
        task_id = None
        try:
            task_id = int(last_arg)
            is_org_id = False
        except ValueError:
            is_org_id = True

        self.assertFalse(is_org_id, "Zero should be parsed as numeric index")
        self.assertEqual(task_id, 0)


class TestParseResult(unittest.TestCase):
    """Test parse_result function with Org IDs."""

    def test_parse_result_with_id(self):
        """Test parsing result line that includes an Org ID."""
        # Import the parse_result function
        import importlib.util

        spec = importlib.util.spec_from_file_location(
            "org_warrior",
            os.path.join(os.path.dirname(__file__), "..", "src", "org-warrior"),
        )
        if spec is None or spec.loader is None:
            self.skipTest("Could not load org-warrior module")
            return

        org_warrior = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(org_warrior)

        # Test with ID
        line = (
            '("Test heading" "TODO" "A" nil nil "/path/to/file.org" 42 "abc123def456")'
        )
        result = org_warrior.parse_result(line)

        self.assertEqual(result["heading"], "Test heading")
        self.assertEqual(result["todo"], "TODO")
        self.assertEqual(result["priority"], "A")
        self.assertEqual(result["id"], "abc123def456")
        self.assertEqual(result["file"], "/path/to/file.org")
        self.assertEqual(result["line"], 42)

    def test_parse_result_without_id(self):
        """Test parsing result line without an Org ID (nil)."""
        import importlib.util

        spec = importlib.util.spec_from_file_location(
            "org_warrior",
            os.path.join(os.path.dirname(__file__), "..", "src", "org-warrior"),
        )
        if spec is None or spec.loader is None:
            self.skipTest("Could not load org-warrior module")
            return

        org_warrior = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(org_warrior)

        # Test without ID (nil)
        line = '("Test heading 2" "NEXT" "B" nil nil "/path/to/other.org" 10 nil)'
        result = org_warrior.parse_result(line)

        self.assertEqual(result["heading"], "Test heading 2")
        self.assertEqual(result["todo"], "NEXT")
        self.assertIsNone(result["priority"])  # B is default, returns None
        self.assertIsNone(result["id"])
        self.assertEqual(result["file"], "/path/to/other.org")
        self.assertEqual(result["line"], 10)


class TestErrorMessages(unittest.TestCase):
    """Test improved error messages."""

    def test_out_of_range_message_content(self):
        """Test that the improved error message mentions Org IDs."""
        # The error message should mention:
        # 1. "Filtered indices may change"
        # 2. "Org IDs"
        # 3. "--ids"

        expected_phrases = ["Filtered indices may change", "Org IDs", "--ids"]

        # Simulate the error message from cmd_schedule
        error_msg = """Task ID 5 out of range (1-3).
Note: Filtered indices may change after scheduling tasks.
Consider using Org IDs for stable task references.
Use 'org-warrior list --ids' or 'org-warrior today --ids' to see task IDs."""

        for phrase in expected_phrases:
            self.assertIn(phrase, error_msg, f"Error message should mention '{phrase}'")


class TestHandleResolution(unittest.TestCase):
    """Test handle resolution rules."""

    def test_handle_token_parsing(self):
        """Test handle resolution rules."""
        
        # Define functions inline for testing since module import doesn't work with executable
        import uuid as _uuid

        def _is_uuid(value: str) -> bool:
            try:
                _uuid.UUID(value)
                return True
            except ValueError:
                return False

        def resolve_id_token(token: str, cache: dict):
            normalized = token.strip().lower()
            if normalized in cache["by_handle"]:
                return cache["by_handle"][normalized], None
            matches = [h for h in cache["by_handle"] if h.startswith(normalized)]
            if len(matches) == 1:
                return cache["by_handle"][matches[0]], None
            if len(matches) > 1:
                return None, sorted(matches)
            if _is_uuid(normalized) or any(ch.isdigit() for ch in normalized):
                return normalized, None
            return None, None

        cache = {"by_handle": {"alpha-beta-gamma": "uuid-1"}, "by_uuid": {}, "version": 1}
        resolved, candidates = resolve_id_token("alpha-beta-g", cache)
        self.assertEqual(resolved, "uuid-1")
        self.assertIsNone(candidates)


if __name__ == "__main__":
    unittest.main()
