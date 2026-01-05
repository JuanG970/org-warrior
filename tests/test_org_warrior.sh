#!/usr/bin/env bash
# Integration tests for org-warrior
# Run with: ./test_org_warrior.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORG_WARRIOR="${SCRIPT_DIR}/org-warrior"
TEST_DIR="/tmp/org-warrior-test-$$"
FAILED=0
PASSED=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

# Dynamic dates for reproducible relative tests
readarray -t DATE_VARS < <(python3 - <<'PY'
from datetime import datetime, date, timedelta
fmt="<%Y-%m-%d %a>"
closed_fmt="[%Y-%m-%d %a %H:%M]"
today = date.today()
print(today.strftime(fmt))                   # 0 TODAY
print((today + timedelta(days=7)).strftime(fmt))  # 1 IN_WEEK (unused but available)
print((today - timedelta(days=10)).strftime(fmt)) # 2 OVERDUE
print((datetime.now() - timedelta(days=2)).strftime(closed_fmt))  # 3 CLOSED_RECENT
print((datetime.now() - timedelta(days=15)).strftime(closed_fmt)) # 4 CLOSED_OLD
PY
)
TODAY="${DATE_VARS[0]}"
OVERDUE="${DATE_VARS[2]}"
CLOSED_RECENT="${DATE_VARS[3]}"
CLOSED_OLD="${DATE_VARS[4]}"

setup() {
    mkdir -p "$TEST_DIR"
    TEST_DIR=$(realpath "$TEST_DIR")  # Resolve symlinks (e.g., /tmp -> /private/tmp on macOS)
    cat > "$TEST_DIR/test.org" << EOF
* TODO Buy groceries :home:
DEADLINE: $TODAY
* TODO [#A] Finish report :work:
SCHEDULED: $TODAY
* NEXT Call dentist :health:
* DONE Review PR :work:
CLOSED: $CLOSED_RECENT
* TODO [#B] Read book :personal:
* WAITING Response from client :work:
* PROJECT Build website :work:
** NEXT Design homepage
** TODO Implement backend
* PROJECT Home renovation :home:
** TODO Paint walls
* TODO [#C] Clean garage :home:
DEADLINE: $OVERDUE
* DONE Old task
CLOSED: $CLOSED_OLD
EOF
    mkdir -p "$TEST_DIR/sub"
    cat > "$TEST_DIR/sub/nested.org" << EOF
* NEXT Nested action :nested:
DEADLINE: $TODAY
* TODO Nested todo :nested:
EOF
    export ORG_WARRIOR_FILES="$TEST_DIR"
}

teardown() {
    rm -rf "$TEST_DIR"
}

assert_contains() {
    local output="$1"
    local expected="$2"
    local test_name="$3"
    
    if echo "$output" | grep -q "$expected"; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((PASSED++))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo "  Expected to find: $expected"
        echo "  Got: $output"
        ((FAILED++))
    fi
}

assert_count() {
    local output="$1"
    local expected="$2"
    local test_name="$3"
    
    if echo "$output" | grep -q "$expected task(s)"; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((PASSED++))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo "  Expected: $expected task(s)"
        echo "  Got: $output"
        ((FAILED++))
    fi
}

# Test: Help output
test_help() {
    local output
    output=$("$ORG_WARRIOR" --help 2>&1)
    assert_contains "$output" "org-warrior - TaskWarrior-like interface" "help displays usage"
    assert_contains "$output" "COMMANDS:" "help shows commands"
}

# Test: List all open tasks
test_list() {
    local output
    output=$("$ORG_WARRIOR" list 2>&1)
    assert_contains "$output" "Buy groceries" "list shows TODO tasks"
    assert_contains "$output" "Finish report" "list shows TODO with priority"
    assert_contains "$output" "Nested action" "list includes nested org files"
    assert_count "$output" "13" "list returns correct count"
}

test_task_detail() {
    local output
    output=$("$ORG_WARRIOR" task 1 2>&1)
    assert_contains "$output" "Task 1:" "task shows header with id"
    assert_contains "$output" "Buy groceries" "task shows correct heading"
    assert_contains "$output" "Location: ${TEST_DIR}/test.org:" "task shows file and line"
}

# Test: Next actions
test_next() {
    local output
    output=$("$ORG_WARRIOR" next 2>&1)
    assert_contains "$output" "Call dentist" "next shows NEXT tasks"
    assert_contains "$output" "Finish report" "next shows priority A TODOs"
    assert_contains "$output" "Nested action" "next includes nested NEXT"
    assert_count "$output" "4" "next returns correct count"
}

# Test: Today's tasks
test_today() {
    local output
    output=$("$ORG_WARRIOR" today 2>&1)
    assert_contains "$output" "Buy groceries" "today shows deadline today"
    assert_contains "$output" "Finish report" "today shows scheduled today"
    assert_contains "$output" "Nested action" "today includes nested deadlines"
    assert_count "$output" "3" "today returns correct count"
}

# Test: Overdue tasks
test_overdue() {
    local output
    output=$("$ORG_WARRIOR" overdue 2>&1)
    assert_contains "$output" "Clean garage" "overdue shows past deadline"
    assert_count "$output" "1" "overdue returns correct count"
}

# Test: Done tasks
test_done() {
    local output
    output=$("$ORG_WARRIOR" done 2>&1)
    assert_contains "$output" "Review PR" "done shows recently closed"
}

# Test: Projects
test_projects() {
    local output
    output=$("$ORG_WARRIOR" projects 2>&1)
    assert_contains "$output" "Build website" "projects shows PROJECT items"
    assert_contains "$output" "Home renovation" "projects shows all projects"
    assert_count "$output" "2" "projects returns correct count"
}

# Test: Stuck projects
test_stuck() {
    local output
    output=$("$ORG_WARRIOR" stuck 2>&1)
    assert_contains "$output" "Home renovation" "stuck shows project without NEXT"
    assert_count "$output" "1" "stuck returns correct count"
}

# Test: Waiting tasks
test_waiting() {
    local output
    output=$("$ORG_WARRIOR" waiting 2>&1)
    assert_contains "$output" "Response from client" "waiting shows WAITING tasks"
    assert_count "$output" "1" "waiting returns correct count"
}

# Test: Tag filter
test_tag() {
    local output
    output=$("$ORG_WARRIOR" tag work 2>&1)
    assert_contains "$output" "Finish report" "tag filters by tag"
    assert_contains "$output" "Build website" "tag shows work projects"
}

# Test: Priority filter
test_priority() {
    local output
    output=$("$ORG_WARRIOR" priority A 2>&1)
    assert_contains "$output" "Finish report" "priority A shows correct task"
    assert_count "$output" "1" "priority returns correct count"
}

# Test: Search
test_search() {
    local output
    output=$("$ORG_WARRIOR" search dentist 2>&1)
    assert_contains "$output" "Call dentist" "search finds matching task"
    assert_count "$output" "1" "search returns correct count"
}

# Test: Tag filter with +
test_tag_filter() {
    local output
    output=$("$ORG_WARRIOR" +home 2>&1)
    assert_contains "$output" "Buy groceries" "tag filter with + works"
    assert_contains "$output" "Home renovation" "tag filter shows all tagged items"
}

# Test: Raw query
test_query() {
    local output
    output=$("$ORG_WARRIOR" query '(todo "WAITING")' 2>&1)
    assert_contains "$output" "Response from client" "raw query works"
    assert_count "$output" "1" "raw query returns correct count"
}

# Main
main() {
    echo "Setting up test environment..."
    setup
    
    echo ""
    echo "Running org-warrior integration tests..."
    echo "========================================="
    
    test_help
    test_list
    test_task_detail
    test_next
    test_today
    test_overdue
    test_done
    test_projects
    test_stuck
    test_waiting
    test_tag
    test_priority
    test_search
    test_tag_filter
    test_query
    
    echo ""
    echo "========================================="
    echo -e "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}"
    
    teardown
    
    if [ $FAILED -gt 0 ]; then
        exit 1
    fi
}

main
