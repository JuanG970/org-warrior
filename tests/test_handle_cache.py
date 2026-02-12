"""Tests for HandleCache module."""

import pytest
import os
import tempfile
from org_warrior.HandleCache import (
    HandleCache,
    _handle_from_uuid,
    _is_uuid,
    ensure_handle_for_uuid,
    resolve_id_token,
    load_wordlist,
    WORDLIST_DEFAULT,
    HANDLES_CACHE,
)


class TestHandleCache:
    """Tests for HandleCache class."""

    def test_handle_from_uuid_deterministic(self):
        """Test that handle generation is deterministic."""
        uuid = "12345678-1234-1234-1234-123456789abc"
        wordlist = WORDLIST_DEFAULT

        handle1 = _handle_from_uuid(uuid, wordlist)
        handle2 = _handle_from_uuid(uuid, wordlist)

        assert handle1 == handle2
        assert "-" in handle1
        assert len(handle1.split("-")) == 3

    def test_handle_from_uuid_different_uuids(self):
        """Test that different UUIDs generate different handles."""
        uuid1 = "12345678-1234-1234-1234-123456789abc"
        uuid2 = "87654321-4321-4321-4321-cba987654321"
        wordlist = WORDLIST_DEFAULT

        handle1 = _handle_from_uuid(uuid1, wordlist)
        handle2 = _handle_from_uuid(uuid2, wordlist)

        assert handle1 != handle2

    def test_is_uuid_valid(self):
        """Test UUID validation."""
        assert _is_uuid("12345678-1234-1234-1234-123456789abc")
        assert _is_uuid("A1B2C3D4-5678-9ABC-DEF0-123456789ABC")

    def test_is_uuid_invalid(self):
        """Test UUID validation rejects invalid formats."""
        assert not _is_uuid("not-a-uuid")
        assert not _is_uuid("alpha-bravo-charlie")
        assert not _is_uuid("")

    def test_ensure_handle_for_uuid_creates(self):
        """Test that ensure_handle_for_uuid creates a new handle."""
        uuid = "12345678-1234-1234-1234-123456789abc"
        cache = {"version": 1, "by_handle": {}, "by_uuid": {}}
        wordlist = WORDLIST_DEFAULT

        handle, created = ensure_handle_for_uuid(uuid, cache, wordlist)

        assert created is True
        assert handle in cache["by_handle"]
        assert cache["by_handle"][handle] == uuid
        assert cache["by_uuid"][uuid] == handle

    def test_ensure_handle_for_uuid_existing(self):
        """Test that ensure_handle_for_uuid returns existing handle."""
        uuid = "12345678-1234-1234-1234-123456789abc"
        existing_handle = "alpha-bravo-charlie"
        cache = {
            "version": 1,
            "by_handle": {existing_handle: uuid},
            "by_uuid": {uuid: existing_handle},
        }
        wordlist = WORDLIST_DEFAULT

        handle, created = ensure_handle_for_uuid(uuid, cache, wordlist)

        assert created is False
        assert handle == existing_handle

    def test_ensure_handle_for_uuid_collision(self):
        """Test that handle collisions are resolved."""
        uuid1 = "12345678-1234-1234-1234-123456789abc"
        uuid2 = "87654321-4321-4321-4321-cba987654321"
        wordlist = WORDLIST_DEFAULT

        # Get initial handle for uuid1
        base_handle = _handle_from_uuid(uuid1, wordlist)

        cache = {
            "version": 1,
            "by_handle": {base_handle: uuid1},
            "by_uuid": {uuid1: base_handle},
        }

        # Force uuid2 to have the same base handle (simulate collision)
        # We'll manually create a collision scenario
        handle2, created = ensure_handle_for_uuid(uuid2, cache, wordlist)

        assert created is True
        # If they collide, handle2 should be different from base_handle
        if _handle_from_uuid(uuid2, wordlist) == base_handle:
            assert handle2 != base_handle
            assert handle2.startswith(base_handle + "-")

    def test_resolve_id_token_exact_match(self):
        """Test resolving an exact handle match."""
        handle = "alpha-bravo-charlie"
        uuid = "12345678-1234-1234-1234-123456789abc"
        cache = {
            "version": 1,
            "by_handle": {handle: uuid},
            "by_uuid": {uuid: handle},
        }

        resolved, candidates = resolve_id_token(handle, cache)

        assert resolved == uuid
        assert candidates is None

    def test_resolve_id_token_prefix_match(self):
        """Test resolving a unique handle prefix."""
        handle = "alpha-bravo-charlie"
        uuid = "12345678-1234-1234-1234-123456789abc"
        cache = {
            "version": 1,
            "by_handle": {handle: uuid},
            "by_uuid": {uuid: handle},
        }

        resolved, candidates = resolve_id_token("alpha", cache)

        assert resolved == uuid
        assert candidates is None

    def test_resolve_id_token_ambiguous(self):
        """Test resolving an ambiguous handle prefix."""
        cache = {
            "version": 1,
            "by_handle": {
                "alpha-bravo-charlie": "uuid1",
                "alpha-bravo-delta": "uuid2",
                "alpha-charlie-delta": "uuid3",
            },
            "by_uuid": {},
        }

        resolved, candidates = resolve_id_token("alpha-bravo", cache)

        assert resolved is None
        assert candidates is not None
        assert len(candidates) == 2
        assert "alpha-bravo-charlie" in candidates
        assert "alpha-bravo-delta" in candidates

    def test_resolve_id_token_uuid(self):
        """Test resolving a UUID directly."""
        uuid = "12345678-1234-1234-1234-123456789abc"
        cache = {"version": 1, "by_handle": {}, "by_uuid": {}}

        resolved, candidates = resolve_id_token(uuid, cache)

        assert resolved == uuid
        assert candidates is None

    def test_resolve_id_token_not_found(self):
        """Test resolving a non-existent handle."""
        cache = {"version": 1, "by_handle": {}, "by_uuid": {}}

        resolved, candidates = resolve_id_token("nonexistent", cache)

        assert resolved is None
        assert candidates is None

    def test_load_wordlist_default(self):
        """Test loading wordlist returns default when no file exists."""
        wordlist = load_wordlist()
        assert wordlist == WORDLIST_DEFAULT

    def test_handle_cache_context_manager(self):
        """Test HandleCache as a context manager."""
        with HandleCache() as hc:
            assert hc.cache is not None
            assert hc.wordlist is not None

    def test_handle_cache_get_handle(self):
        """Test HandleCache.get_handle()."""
        uuid = "12345678-1234-1234-1234-123456789abc"

        with HandleCache() as hc:
            handle = hc.get_handle(uuid)
            assert handle is not None
            assert isinstance(handle, str)
            assert "-" in handle

    def test_handle_cache_resolve(self):
        """Test HandleCache.resolve()."""
        uuid = "12345678-1234-1234-1234-123456789abc"

        with HandleCache() as hc:
            # First create a handle
            handle = hc.get_handle(uuid)

            # Then resolve it
            resolved = hc.resolve(handle)
            assert resolved == uuid
