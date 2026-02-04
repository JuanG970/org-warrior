#!/usr/bin/env bash
# Integration tests for org-warrior
# Run with: ./test_org_warrior.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORG_WARRIOR="${SCRIPT_DIR}/../src/org-warrior"
TEST_DIR="/tmp/org-warrior-test-$$"
FAILED=0
PASSED=0
EMACS_PID=""
export ORG_WARRIOR_SERVER="org-warrior-test-$$"

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
    start_emacs_daemon || exit 1
    cat > "$TEST_DIR/test.org" << EOF
* TODO Buy groceries :home:
DEADLINE: $TODAY
* TODO [#A] Finish report :work:
SCHEDULED: $TODAY
* STRT Call dentist :health:
* DONE Review PR :work:
CLOSED: $CLOSED_RECENT
* TODO [#B] Read book :personal:
* WAIT Response from client :work:
* TODO Build website :work:project:
** STRT Design homepage
** TODO Implement backend
* TODO Home renovation :home:project:
** TODO Paint walls
* TODO [#C] Clean garage :home:
DEADLINE: $OVERDUE
* TODO Pay rent :finance:
SCHEDULED: $OVERDUE
* DONE Old task
CLOSED: $CLOSED_OLD
EOF
    mkdir -p "$TEST_DIR/sub"
    cat > "$TEST_DIR/sub/nested.org" << EOF
* STRT Nested action :nested:
DEADLINE: $TODAY
* TODO Nested todo :nested:
EOF
    export ORG_WARRIOR_FILES="$TEST_DIR"
    "$ORG_WARRIOR" handles-assign >/dev/null 2>&1
}

teardown() {
    rm -rf "$TEST_DIR"
    if [ -n "$EMACS_PID" ]; then
        kill "$EMACS_PID" >/dev/null 2>&1 || true
    fi
}

assert_contains() {
    local output="$1"
    local expected="$2"
    local test_name="$3"
    
    if echo "$output" | grep -q -- "$expected"; then
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

start_emacs_daemon() {
    if emacsclient -s "$ORG_WARRIOR_SERVER" -e '(+ 1 1)' >/dev/null 2>&1; then
        return 0
    fi
    emacs --daemon="$ORG_WARRIOR_SERVER" >/dev/null 2>&1
    for _ in {1..60}; do
        if emacsclient -s "$ORG_WARRIOR_SERVER" -e '(+ 1 1)' >/dev/null 2>&1; then
            return 0
        fi
        sleep 1
    done
    echo "Error: Emacs daemon failed to start." >&2
    return 1
}

get_sample_handle() {
    python3 - <<'PY'
import json
import os
path = os.path.expanduser("~/.org-warrior/handles.json")
try:
    with open(path, "r", encoding="utf-8") as fh:
        data = json.load(fh)
    handles = list(data.get("by_handle", {}).keys())
    print(handles[0] if handles else "")
except FileNotFoundError:
    print("")
PY
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
    assert_count "$output" "14" "list returns correct count"
}

test_task_detail() {
    local output
    output=$("$ORG_WARRIOR" task 1 2>&1)
    assert_contains "$output" "Task 1:" "task shows header with id"
    assert_contains "$output" "Buy groceries" "task shows correct heading"
    assert_contains "$output" "Location: ${TEST_DIR}/test.org:" "task shows file and line"
    assert_contains "$output" "Handle:" "task shows handle"
    assert_contains "$output" "ID:" "task shows ID"
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
    assert_contains "$output" "Clean garage" "today includes overdue deadlines"
    assert_contains "$output" "Pay rent" "today includes overdue scheduled"
    assert_count "$output" "5" "today returns correct count"
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
    output=$("$ORG_WARRIOR" query '(todo "WAIT")' 2>&1)
    assert_contains "$output" "Response from client" "raw query works"
    assert_count "$output" "1" "raw query returns correct count"
}

# Test: --ids flag with list
test_list_with_ids() {
    local output
    output=$("$ORG_WARRIOR" list --ids 2>&1)
    assert_contains "$output" "ID:" "list --ids shows ID labels"
}

test_list_handles_default() {
    local output
    local handle
    output=$("$ORG_WARRIOR" list 2>&1)
    handle=$(get_sample_handle)
    if [ -z "$handle" ]; then
        echo -e "${RED}✗${NC} list shows handles by default"
        echo "  Expected to find a handle in cache"
        ((FAILED++))
        return
    fi
    assert_contains "$output" "$handle" "list shows handles by default"
}

test_list_no_handles() {
    local output
    local handle
    output=$("$ORG_WARRIOR" list --no-handles 2>&1)
    handle=$(get_sample_handle)
    if [ -n "$handle" ] && echo "$output" | grep -q -- "$handle"; then
        echo -e "${RED}✗${NC} list --no-handles hides handles"
        ((FAILED++))
    else
        echo -e "${GREEN}✓${NC} list --no-handles hides handles"
        ((PASSED++))
    fi
}

test_handles_assign() {
    local output
    output=$("$ORG_WARRIOR" handles-assign 2>&1)
    assert_contains "$output" "Handles cached" "handles-assign populates cache"
}

# Test: --ids flag with today
test_today_with_ids() {
    local output
    output=$("$ORG_WARRIOR" today --ids 2>&1)
    assert_contains "$output" "ID:" "today --ids shows ID labels"
}

# Test: Improved error message for out-of-range
test_out_of_range_error() {
    local output
    output=$("$ORG_WARRIOR" schedule "6pm" today 999 2>&1)
    assert_contains "$output" "out of range" "error shows out of range"
    assert_contains "$output" "Filtered indices may change" "error mentions filtered indices"
    assert_contains "$output" "Org IDs" "error mentions Org IDs"
    assert_contains "$output" "--ids" "error mentions --ids flag"
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
    test_list_with_ids
    test_list_handles_default
    test_list_no_handles
    test_handles_assign
    test_today_with_ids
    test_out_of_range_error
    
    echo ""
    echo "========================================="
    echo -e "Results: ${GREEN}$PASSED passed${NC}, ${RED}$FAILED failed${NC}"
    
    teardown
    
    if [ $FAILED -gt 0 ]; then
        exit 1
    fi
}

main
