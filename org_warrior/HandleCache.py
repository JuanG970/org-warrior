"""Handle cache system for mapping Org IDs to human-readable handles."""

import hashlib
import json
import os
import sys
import tempfile
import uuid as _uuid
from typing import Optional, Tuple


# Configuration
HANDLES_CACHE = os.path.expanduser("~/.org-warrior/handles.json")
WORDLIST_PATHS = [
    os.path.join(os.path.dirname(os.path.abspath(__file__)), "wordlist.txt"),
    os.path.expanduser("~/.org-warrior/wordlist.txt"),
]
WORDLIST_DEFAULT = [
    "alpha",
    "bravo",
    "charlie",
    "delta",
    "echo",
    "foxtrot",
    "golf",
    "hotel",
    "india",
    "juliet",
    "kilo",
    "lima",
    "mike",
    "november",
    "oscar",
    "papa",
]
HANDLE_WORDS = 3
HANDLE_VERSION = 1


def load_wordlist() -> list[str]:
    """Load wordlist from file or return default."""
    for path in WORDLIST_PATHS:
        if os.path.isfile(path):
            with open(path, "r", encoding="utf-8") as fh:
                words = [w.strip().lower() for w in fh if w.strip()]
            if words and (len(words) & (len(words) - 1) == 0):
                return words
            print(
                f"Warning: wordlist length must be power of two: {path}",
                file=sys.stderr,
            )
    print("Warning: using fallback wordlist; handles may collide.", file=sys.stderr)
    return WORDLIST_DEFAULT


def _empty_handle_cache() -> dict:
    """Create an empty handle cache structure."""
    return {"version": HANDLE_VERSION, "by_handle": {}, "by_uuid": {}}


def load_handle_cache() -> dict:
    """Load handle cache from disk or return empty cache."""
    if not os.path.isfile(HANDLES_CACHE):
        return _empty_handle_cache()
    try:
        with open(HANDLES_CACHE, "r", encoding="utf-8") as fh:
            data = json.load(fh)
        if not isinstance(data, dict):
            return _empty_handle_cache()
        if "by_handle" not in data or "by_uuid" not in data:
            return _empty_handle_cache()
        return data
    except (OSError, json.JSONDecodeError):
        return _empty_handle_cache()


def save_handle_cache(cache: dict) -> None:
    """Save handle cache to disk atomically."""
    os.makedirs(os.path.dirname(HANDLES_CACHE), exist_ok=True)
    fd, tmp_path = tempfile.mkstemp(
        prefix="handles-", dir=os.path.dirname(HANDLES_CACHE)
    )
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as fh:
            json.dump(cache, fh, indent=2, sort_keys=True)
        os.replace(tmp_path, HANDLES_CACHE)
    finally:
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def _is_uuid(value: str) -> bool:
    """Check if a string is a valid UUID."""
    try:
        _uuid.UUID(value)
        return True
    except ValueError:
        return False


def _word_indices_from_hash(hbytes: bytes, word_count: int) -> list[int]:
    """Extract word indices from hash bytes."""
    bits = int.from_bytes(hbytes, "big")
    word_bits = word_count.bit_length() - 1
    mask = (1 << word_bits) - 1
    indices = []
    for i in range(HANDLE_WORDS):
        shift = i * word_bits
        indices.append((bits >> shift) & mask)
    return indices


def _handle_from_uuid(uuid_str: str, wordlist: list[str]) -> str:
    """Generate a deterministic handle from a UUID."""
    hbytes = hashlib.sha256(uuid_str.encode("utf-8")).digest()
    indices = _word_indices_from_hash(hbytes, len(wordlist))
    return "-".join(wordlist[i % len(wordlist)] for i in indices)


def ensure_handle_for_uuid(
    uuid_str: str, cache: dict, wordlist: list[str]
) -> Tuple[str, bool]:
    """
    Ensure a handle exists for a UUID.

    Returns:
        Tuple of (handle, created) where created is True if a new handle was generated.
    """
    existing = cache["by_uuid"].get(uuid_str)
    if existing:
        return existing, False
    base_handle = _handle_from_uuid(uuid_str, wordlist)
    handle = base_handle
    counter = 1
    while handle in cache["by_handle"] and cache["by_handle"][handle] != uuid_str:
        handle = f"{base_handle}-{counter}"
        counter += 1
    cache["by_uuid"][uuid_str] = handle
    cache["by_handle"][handle] = uuid_str
    return handle, True


def resolve_id_token(token: str, cache: dict) -> Tuple[Optional[str], Optional[list]]:
    """
    Resolve a handle/org-id token to an Org ID.

    Returns:
        Tuple of (org_id, candidates) where:
        - If exact match: (org_id, None)
        - If ambiguous prefix: (None, [candidate_handles])
        - If not found: (None, None)
    """
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


class HandleCache:
    """Manages handle cache for Org IDs."""

    def __init__(self):
        self.cache = load_handle_cache()
        self.wordlist = load_wordlist()
        self._dirty = False

    def get_handle(self, org_id: str) -> str:
        """Get handle for an Org ID, creating one if needed."""
        handle, created = ensure_handle_for_uuid(org_id, self.cache, self.wordlist)
        if created:
            self._dirty = True
        return handle

    def resolve(self, token: str) -> Optional[str]:
        """
        Resolve a handle or Org ID to an Org ID.

        Returns None if token is ambiguous or not found.
        Prints error messages to stderr for user feedback.
        """
        resolved, candidates = resolve_id_token(token, self.cache)
        if candidates:
            print("Ambiguous handle prefix. Candidates:", file=sys.stderr)
            for cand in candidates:
                print(f"  {cand}", file=sys.stderr)
            return None
        if not resolved:
            print(
                "Unknown handle. Run 'org-warrior list' to see handles or use full Org ID.",
                file=sys.stderr,
            )
            return None
        return resolved

    def save(self) -> None:
        """Save cache to disk if modified."""
        if self._dirty:
            save_handle_cache(self.cache)
            self._dirty = False

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - auto-save if dirty."""
        self.save()
        return False
