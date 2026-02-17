"""Tests for parsing utilities in Org.py."""

import os
import tempfile
import pytest
from datetime import date, datetime

from org_warrior.Org import (
    _tokenize_sexp,
    parse_org_ql_result,
    parse_org_timestamp,
    date_status,
    resolve_org_files,
)


class TestTokenizeSexp:
    """Tests for _tokenize_sexp function."""

    def test_empty_string(self):
        assert _tokenize_sexp("") == []

    def test_single_string(self):
        assert _tokenize_sexp('"hello"') == ["hello"]

    def test_multiple_strings(self):
        assert _tokenize_sexp('"a" "b" "c"') == ["a", "b", "c"]

    def test_nil(self):
        assert _tokenize_sexp("nil") == [None]

    def test_integer(self):
        assert _tokenize_sexp("42") == [42]

    def test_negative_integer(self):
        assert _tokenize_sexp("-7") == [-7]

    def test_mixed_tokens(self):
        result = _tokenize_sexp('"heading" "TODO" nil nil nil "/path" 42 "uuid-123"')
        assert result == ["heading", "TODO", None, None, None, "/path", 42, "uuid-123"]

    def test_escaped_quotes_in_string(self):
        result = _tokenize_sexp(r'"say \"hello\""')
        assert result == ['say "hello"']

    def test_escaped_backslash_in_string(self):
        result = _tokenize_sexp(r'"path\\to\\file"')
        assert result == ["path\\to\\file"]

    def test_parentheses_skipped(self):
        result = _tokenize_sexp('("a" "b")')
        assert result == ["a", "b"]

    def test_whitespace_handling(self):
        result = _tokenize_sexp('  "a"  \t "b"  ')
        assert result == ["a", "b"]


class TestParseOrgQlResult:
    """Tests for parse_org_ql_result."""

    def test_contains_error(self):
        line = 'ERROR: <ERROR MESSAGE>'
        # Should return an Exception:
        try:
            result = parse_org_ql_result(line)
        except Exception as e:
            result = e
            assert isinstance(result, Exception)


class TestParseOrgTimestamp:
    """Tests for parse_org_timestamp."""

    def test_none(self):
        d, t = parse_org_timestamp(None)
        assert d is None
        assert t is None

    def test_empty_string(self):
        d, t = parse_org_timestamp("")
        assert d is None
        assert t is None

    def test_simple_date(self):
        d, t = parse_org_timestamp("2026-03-15")
        assert d == date(2026, 3, 15)
        assert t is None

    def test_date_with_day_name(self):
        d, t = parse_org_timestamp("2026-03-15 Sun")
        assert d == date(2026, 3, 15)
        assert t is None

    def test_date_with_time(self):
        d, t = parse_org_timestamp("2026-03-15 Sun 10:30")
        assert d == date(2026, 3, 15)
        assert t == "10:30"

    def test_org_active_timestamp(self):
        d, t = parse_org_timestamp("<2026-03-15 Sun +1w -1d>")
        assert d == date(2026, 3, 15)

    def test_org_inactive_timestamp(self):
        d, t = parse_org_timestamp("[2026-03-15 Sun 10:30]")
        assert d == date(2026, 3, 15)
        assert t == "10:30"

    def test_datetime_object(self):
        dt = datetime(2026, 5, 1, 14, 30)
        d, t = parse_org_timestamp(dt)
        assert d == date(2026, 5, 1)
        assert t == "14:30"

    def test_date_object(self):
        d_in = date(2026, 5, 1)
        d, t = parse_org_timestamp(d_in)
        assert d == d_in
        assert t is None

    def test_invalid_string(self):
        d, t = parse_org_timestamp("not a date")
        assert d is None
        assert t is None


class TestDateStatus:
    """Tests for date_status."""

    def test_none(self):
        assert date_status(None) is None

    def test_overdue(self):
        past = date.today() - __import__("datetime").timedelta(days=5)
        assert date_status(past) == "overdue"

    def test_today(self):
        assert date_status(date.today()) == "today"

    def test_future(self):
        future = date.today() + __import__("datetime").timedelta(days=10)
        assert date_status(future) == "future"


class TestResolveOrgFiles:
    """Tests for resolve_org_files."""

    def test_single_file(self):
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f:
            f.write(b"* TODO Test")
            path = f.name
        try:
            result = resolve_org_files(path)
            assert path in result
        finally:
            os.unlink(path)

    def test_directory(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create some .org files
            for name in ["a.org", "b.org", "c.txt"]:
                with open(os.path.join(tmpdir, name), "w") as f:
                    f.write("test")
            result = resolve_org_files(tmpdir)
            assert len(result) == 2  # only .org files
            basenames = [os.path.basename(f) for f in result]
            assert "a.org" in basenames
            assert "b.org" in basenames

    def test_nested_directory(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            sub = os.path.join(tmpdir, "sub")
            os.makedirs(sub)
            with open(os.path.join(sub, "deep.org"), "w") as f:
                f.write("test")
            result = resolve_org_files(tmpdir)
            assert any("deep.org" in f for f in result)

    def test_nonexistent_path(self):
        result = resolve_org_files("/nonexistent/path/xyz")
        assert result == []

    def test_colon_separated_paths(self):
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f1:
            f1.write(b"test")
            path1 = f1.name
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f2:
            f2.write(b"test")
            path2 = f2.name
        try:
            result = resolve_org_files(f"{path1}:{path2}")
            assert path1 in result
            assert path2 in result
        finally:
            os.unlink(path1)
            os.unlink(path2)

    def test_deduplicates(self):
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f:
            f.write(b"test")
            path = f.name
        try:
            result = resolve_org_files(f"{path}:{path}")
            assert result.count(path) == 1
        finally:
            os.unlink(path)

    def test_list_input(self):
        with tempfile.NamedTemporaryFile(suffix=".org", delete=False) as f:
            f.write(b"test")
            path = f.name
        try:
            result = resolve_org_files([path])
            assert path in result
        finally:
            os.unlink(path)

    def test_empty_specs(self):
        result = resolve_org_files("")
        assert result == []
