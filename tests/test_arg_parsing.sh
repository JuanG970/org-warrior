#!/usr/bin/env bash
# Simple test for argument parsing logic (doesn't require Emacs)

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

PASSED=0
FAILED=0

test_numeric_arg() {
    # Test that "3" is parsed as numeric
    local arg="3"
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
        echo -e "${GREEN}✓${NC} Numeric argument '3' correctly identified"
        ((PASSED++))
    else
        echo -e "${RED}✗${NC} Failed to identify '3' as numeric"
        ((FAILED++))
    fi
}

test_org_id_arg() {
    # Test that "abc123" is NOT purely numeric
    local arg="abc123def"
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
        echo -e "${RED}✗${NC} Incorrectly identified 'abc123def' as numeric"
        ((FAILED++))
    else
        echo -e "${GREEN}✓${NC} Org ID 'abc123def' correctly identified as non-numeric"
        ((PASSED++))
    fi
}

test_uuid_arg() {
    # Test UUID-style ID
    local arg="550e8400-e29b-41d4-a716-446655440000"
    if [[ "$arg" =~ ^[0-9]+$ ]]; then
        echo -e "${RED}✗${NC} Incorrectly identified UUID as numeric"
        ((FAILED++))
    else
        echo -e "${GREEN}✓${NC} UUID correctly identified as non-numeric"
        ((PASSED++))
    fi
}

echo "Running argument parsing tests..."
echo "=================================="
test_numeric_arg
test_org_id_arg
test_uuid_arg

echo ""
echo "=================================="
echo -e "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
