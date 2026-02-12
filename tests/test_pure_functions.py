#!/usr/bin/env python3
"""Unit tests for org-warrior pure functions (no Emacs required)."""

import importlib.machinery
import importlib.util
import os
import sys
import unittest
from datetime import date, timedelta

# Import org-warrior as a module despite lack of .py extension
_src_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "src", "org-warrior")
_loader = importlib.machinery.SourceFileLoader("org_warrior", _src_path)
_spec = importlib.util.spec_from_loader("org_warrior", _loader, origin=_src_path)
ow = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(ow)


# ---------------------------------------------------------------------------
# _escape_elisp
# ---------------------------------------------------------------------------
class TestEscapeElisp(unittest.TestCase):
    def test_plain(self):
        self.assertEqual(ow._escape_elisp("hello"), "hello")

    def test_quotes(self):
        self.assertEqual(ow._escape_elisp('say "hi"'), 'say \\"hi\\"')

    def test_backslashes(self):
        self.assertEqual(ow._escape_elisp("a\\b"), "a\\\\b")

    def test_both(self):
        self.assertEqual(ow._escape_elisp('a\\"b'), 'a\\\\\\"b')

    def test_empty(self):
        self.assertEqual(ow._escape_elisp(""), "")


# ---------------------------------------------------------------------------
# _tokenize_sexp
# ---------------------------------------------------------------------------
class TestTokenizeSexp(unittest.TestCase):
    def test_strings_and_nil(self):
        tokens = ow._tokenize_sexp('"hello" nil "world"')
        self.assertEqual(tokens, ["hello", None, "world"])

    def test_integers(self):
        tokens = ow._tokenize_sexp("42 -7 0")
        self.assertEqual(tokens, [42, -7, 0])

    def test_escaped_quotes_in_string(self):
        tokens = ow._tokenize_sexp(r'"say \"hi\""')
        self.assertEqual(tokens, ['say "hi"'])

    def test_escaped_backslash(self):
        tokens = ow._tokenize_sexp(r'"a\\b"')
        self.assertEqual(tokens, ["a\\b"])

    def test_empty(self):
        self.assertEqual(ow._tokenize_sexp(""), [])

    def test_mixed(self):
        tokens = ow._tokenize_sexp('"foo" 10 nil "bar"')
        self.assertEqual(tokens, ["foo", 10, None, "bar"])


# ---------------------------------------------------------------------------
# _unescape_elisp_string
# ---------------------------------------------------------------------------
class TestUnescapeElispString(unittest.TestCase):
    def test_newline(self):
        self.assertEqual(ow._unescape_elisp_string("a\\nb"), "a\nb")

    def test_tab(self):
        self.assertEqual(ow._unescape_elisp_string("a\\tb"), "a\tb")

    def test_quote(self):
        self.assertEqual(ow._unescape_elisp_string('a\\"b'), 'a"b')

    def test_backslash(self):
        self.assertEqual(ow._unescape_elisp_string("a\\\\b"), "a\\b")

    def test_plain(self):
        self.assertEqual(ow._unescape_elisp_string("hello"), "hello")


# ---------------------------------------------------------------------------
# parse_result
# ---------------------------------------------------------------------------
class TestParseResult(unittest.TestCase):
    def test_full_8_field(self):
        line = '("Buy groceries" "TODO" "A" "<2025-01-01>" "<2025-01-02>" "/tmp/test.org" 42 "uuid-abc")'
        r = ow.parse_result(line)
        self.assertEqual(r["heading"], "Buy groceries")
        self.assertEqual(r["todo"], "TODO")
        self.assertEqual(r["priority"], "A")
        self.assertEqual(r["deadline"], "<2025-01-01>")
        self.assertEqual(r["scheduled"], "<2025-01-02>")
        self.assertEqual(r["file"], "/tmp/test.org")
        self.assertEqual(r["line"], 42)
        self.assertEqual(r["id"], "uuid-abc")

    def test_nil_fields(self):
        line = '("Heading" "NEXT" nil nil nil "/f.org" 1 nil)'
        r = ow.parse_result(line)
        self.assertEqual(r["todo"], "NEXT")
        self.assertIsNone(r["priority"])
        self.assertIsNone(r["deadline"])
        self.assertIsNone(r["id"])

    def test_priority_b_becomes_none(self):
        line = '("H" "TODO" "B" nil nil "/f.org" 1 nil)'
        r = ow.parse_result(line)
        self.assertIsNone(r["priority"])

    def test_escaped_quotes_in_heading(self):
        line = r'("Say \"hello\"" "TODO" nil nil nil "/f.org" 1 nil)'
        r = ow.parse_result(line)
        self.assertEqual(r["heading"], 'Say "hello"')

    def test_malformed_falls_back(self):
        r = ow.parse_result("just plain text")
        self.assertEqual(r, {"heading": "just plain text"})

    def test_short_sexp(self):
        r = ow.parse_result('("only" "two")')
        self.assertEqual(r, {"heading": '("only" "two")'})


# ---------------------------------------------------------------------------
# build_query
# ---------------------------------------------------------------------------
class TestBuildQuery(unittest.TestCase):
    def test_empty(self):
        self.assertEqual(ow.build_query([]), '(and (todo) (not (done)))')

    def test_tag(self):
        q = ow.build_query(["+work"])
        self.assertIn('(tags "work")', q)
        self.assertIn("(not (done))", q)

    def test_priority(self):
        q = ow.build_query(["pri:A"])
        self.assertIn('(priority "A")', q)

    def test_due_today(self):
        q = ow.build_query(["due:today"])
        self.assertIn("(deadline :on today)", q)

    def test_due_week(self):
        q = ow.build_query(["due:week"])
        self.assertIn("(deadline :from today :to +7)", q)

    def test_due_overdue(self):
        q = ow.build_query(["due:overdue"])
        self.assertIn("(deadline :to -1)", q)

    def test_scheduled_today(self):
        q = ow.build_query(["scheduled:today"])
        self.assertIn("(scheduled :on today)", q)

    def test_text_search(self):
        q = ow.build_query(["meeting"])
        self.assertIn('(regexp "meeting")', q)

    def test_project(self):
        q = ow.build_query(["project:"])
        self.assertIn('(ancestors (tags "project"))', q)

    def test_combination(self):
        q = ow.build_query(["+work", "pri:A"])
        self.assertIn('(tags "work")', q)
        self.assertIn('(priority "A")', q)
        self.assertTrue(q.startswith("(and "))

    def test_single_condition_no_and(self):
        q = ow.build_query(["due:today"])
        # Single non-done condition: should still wrap in and with (not (done))
        self.assertIn("(and ", q)

    def test_escaping(self):
        q = ow.build_query(['say "hi"'])
        self.assertIn('\\"', q)


# ---------------------------------------------------------------------------
# format_task
# ---------------------------------------------------------------------------
class TestFormatTask(unittest.TestCase):
    def setUp(self):
        ow.set_color_mode(False)

    def test_basic(self):
        task = {"heading": "Buy milk", "todo": "TODO"}
        out = ow.format_task(task, idx=1, show_handles=False)
        self.assertIn("1", out)
        self.assertIn("[TODO", out)
        self.assertIn("Buy milk", out)

    def test_with_priority(self):
        task = {"heading": "Urgent", "todo": "TODO", "priority": "A"}
        out = ow.format_task(task, show_handles=False)
        self.assertIn("(A)", out)

    def test_with_dates(self):
        task = {"heading": "T", "todo": "TODO", "deadline": "<2025-01-01>", "scheduled": "<2025-01-02>"}
        out = ow.format_task(task, show_handles=False)
        self.assertIn("DUE:", out)
        self.assertIn("SCH:", out)

    def test_show_file(self):
        task = {"heading": "T", "todo": "TODO", "file": "/home/user/org/inbox.org"}
        out = ow.format_task(task, show_handles=False, show_file=True)
        self.assertIn("inbox.org", out)

    def test_handle_display(self):
        task = {"heading": "T", "todo": "TODO", "handle": "alpha-bravo-charlie"}
        out = ow.format_task(task, show_handles=True)
        self.assertIn("alpha-bravo-charlie", out)


# ---------------------------------------------------------------------------
# resolve_org_files
# ---------------------------------------------------------------------------
class TestResolveOrgFiles(unittest.TestCase):
    def test_single_file(self):
        import tempfile
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f:
            f.write(b"* test\n")
            path = f.name
        try:
            result = ow.resolve_org_files(path)
            self.assertEqual(result, [path])
        finally:
            os.unlink(path)

    def test_directory(self):
        import tempfile
        d = tempfile.mkdtemp()
        f1 = os.path.join(d, "a.org")
        f2 = os.path.join(d, "b.txt")
        open(f1, "w").close()
        open(f2, "w").close()
        try:
            result = ow.resolve_org_files(d)
            self.assertEqual(result, [f1])
        finally:
            os.unlink(f1)
            os.unlink(f2)
            os.rmdir(d)

    def test_dedup(self):
        import tempfile
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f:
            path = f.name
        try:
            result = ow.resolve_org_files(f"{path}{os.pathsep}{path}")
            self.assertEqual(len(result), 1)
        finally:
            os.unlink(path)

    def test_mixed(self):
        import tempfile
        d = tempfile.mkdtemp()
        f1 = os.path.join(d, "a.org")
        open(f1, "w").close()
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f2:
            path2 = f2.name
        try:
            result = ow.resolve_org_files(f"{d}{os.pathsep}{path2}")
            self.assertIn(f1, result)
            self.assertIn(path2, result)
        finally:
            os.unlink(f1)
            os.unlink(path2)
            os.rmdir(d)


# ---------------------------------------------------------------------------
# parse_org_timestamp & date_status
# ---------------------------------------------------------------------------
class TestParseOrgTimestamp(unittest.TestCase):
    def test_date_only(self):
        d, t = ow.parse_org_timestamp("<2025-03-15 Sat>")
        self.assertEqual(d, date(2025, 3, 15))
        self.assertIsNone(t)

    def test_date_with_time(self):
        d, t = ow.parse_org_timestamp("<2025-03-15 Sat 14:30>")
        self.assertEqual(d, date(2025, 3, 15))
        self.assertEqual(t, "14:30")

    def test_none_input(self):
        d, t = ow.parse_org_timestamp(None)
        self.assertIsNone(d)
        self.assertIsNone(t)

    def test_empty(self):
        d, t = ow.parse_org_timestamp("")
        self.assertIsNone(d)

    def test_garbage(self):
        d, t = ow.parse_org_timestamp("not a date")
        self.assertIsNone(d)


class TestDateStatus(unittest.TestCase):
    def test_overdue(self):
        yesterday = date.today() - timedelta(days=1)
        self.assertEqual(ow.date_status(yesterday), "overdue")

    def test_today(self):
        self.assertEqual(ow.date_status(date.today()), "today")

    def test_future(self):
        tomorrow = date.today() + timedelta(days=1)
        self.assertEqual(ow.date_status(tomorrow), "future")

    def test_none(self):
        self.assertIsNone(ow.date_status(None))


# ---------------------------------------------------------------------------
# resolve_id_token
# ---------------------------------------------------------------------------
class TestResolveIdToken(unittest.TestCase):
    def setUp(self):
        self.cache = {
            "version": 1,
            "by_handle": {
                "alpha-bravo-charlie": "uuid-1",
                "alpha-bravo-delta": "uuid-2",
                "echo-foxtrot-golf": "uuid-3",
            },
            "by_uuid": {
                "uuid-1": "alpha-bravo-charlie",
                "uuid-2": "alpha-bravo-delta",
                "uuid-3": "echo-foxtrot-golf",
            },
        }

    def test_exact_match(self):
        resolved, candidates = ow.resolve_id_token("alpha-bravo-charlie", self.cache)
        self.assertEqual(resolved, "uuid-1")
        self.assertIsNone(candidates)

    def test_unique_prefix(self):
        resolved, candidates = ow.resolve_id_token("echo", self.cache)
        self.assertEqual(resolved, "uuid-3")
        self.assertIsNone(candidates)

    def test_ambiguous_prefix(self):
        resolved, candidates = ow.resolve_id_token("alpha-bravo", self.cache)
        self.assertIsNone(resolved)
        self.assertEqual(len(candidates), 2)

    def test_uuid_passthrough(self):
        resolved, candidates = ow.resolve_id_token("550e8400-e29b-41d4-a716-446655440000", self.cache)
        self.assertEqual(resolved, "550e8400-e29b-41d4-a716-446655440000")
        self.assertIsNone(candidates)

    def test_unknown(self):
        resolved, candidates = ow.resolve_id_token("zzz-not-found", self.cache)
        self.assertIsNone(resolved)
        self.assertIsNone(candidates)

    def test_case_insensitive(self):
        resolved, candidates = ow.resolve_id_token("Alpha-Bravo-Charlie", self.cache)
        self.assertEqual(resolved, "uuid-1")

    def test_numeric_string_passthrough(self):
        resolved, candidates = ow.resolve_id_token("abc123", self.cache)
        self.assertEqual(resolved, "abc123")


# ---------------------------------------------------------------------------
# _resolve_query_from_filter
# ---------------------------------------------------------------------------
class TestResolveQueryFromFilter(unittest.TestCase):
    def test_empty(self):
        q = ow._resolve_query_from_filter([])
        self.assertIn("(todo)", q)

    def test_today(self):
        q = ow._resolve_query_from_filter(["today"])
        self.assertIn("deadline :to today", q)

    def test_next(self):
        q = ow._resolve_query_from_filter(["next"])
        self.assertIn("STRT", q)

    def test_week(self):
        q = ow._resolve_query_from_filter(["week"])
        self.assertIn("+7", q)

    def test_custom(self):
        q = ow._resolve_query_from_filter(["+work"])
        self.assertIn("work", q)


# ---------------------------------------------------------------------------
# SORT_KEYS
# ---------------------------------------------------------------------------
class TestSortKeys(unittest.TestCase):
    def test_priority_sort(self):
        tasks = [
            {"priority": "C"},
            {"priority": "A"},
            {"priority": None},
        ]
        tasks.sort(key=ow.SORT_KEYS["priority"])
        self.assertEqual(tasks[0]["priority"], "A")
        self.assertEqual(tasks[1]["priority"], None)
        self.assertEqual(tasks[2]["priority"], "C")

    def test_heading_sort(self):
        tasks = [
            {"heading": "Zebra"},
            {"heading": "Apple"},
        ]
        tasks.sort(key=ow.SORT_KEYS["heading"])
        self.assertEqual(tasks[0]["heading"], "Apple")


if __name__ == "__main__":
    unittest.main()
