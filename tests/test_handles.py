#!/usr/bin/env python3

import hashlib

def _word_indices_from_hash(hbytes: bytes, word_count: int) -> list:
    bits = int.from_bytes(hbytes, "big")
    word_bits = word_count.bit_length() - 1
    mask = (1 << word_bits) - 1
    indices = []
    for i in range(3):  # HANDLE_WORDS = 3
        shift = i * word_bits
        indices.append((bits >> shift) & mask)
    return indices

def _handle_from_uuid(uuid_str: str, wordlist: list) -> str:
    hbytes = hashlib.sha256(uuid_str.encode("utf-8")).digest()
    indices = _word_indices_from_hash(hbytes, len(wordlist))
    return "-".join(wordlist[i % len(wordlist)] for i in indices)

# Test with sample data
wordlist = ['alpha', 'bravo', 'charlie', 'delta', 'echo', 'foxtrot', 'golf', 'hotel', 'india', 'juliet', 'kilo', 'lima', 'mike', 'november', 'oscar', 'papa']
test_uuid = 'abc123de-f456-7890-abcd-ef0123456789'

handle1 = _handle_from_uuid(test_uuid, wordlist)
handle2 = _handle_from_uuid(test_uuid, wordlist)

print(f"Handle 1: {handle1}")
print(f"Handle 2: {handle2}")  
print(f"Deterministic: {handle1 == handle2}")

# Test with different UUIDs
test_uuid2 = 'def456ab-c789-0123-4567-890abcdef012'
handle3 = _handle_from_uuid(test_uuid2, wordlist)
print(f"Handle 3: {handle3}")
print(f"Different UUIDs give different handles: {handle1 != handle3}")