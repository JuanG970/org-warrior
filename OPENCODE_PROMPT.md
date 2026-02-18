# Opencode Task: Fix NoneType Error in parse_org_ql_result

## Issue
When `emacs_run_elisp_file` returns `None` (due to connection error, timeout, etc.), `parse_org_ql_result(None)` crashes with:
```
'NoneType' object has no attribute 'startswith'
```

This error message is confusing to users who see "'NoneType' object is not iterable" from the catch-all exception handler.

## Current Code Location
**File**: `org_warrior/Org.py`  
**Function**: `parse_org_ql_result(result: str) -> dict`

The function expects a string but doesn't check for `None` before calling `result.startswith('ERROR:')`.

## Fix Required
1. Add `None` and empty string check at the start of `parse_org_ql_result`
2. Return empty list `[]` when result is `None` or empty string
3. This makes behavior consistent with empty query results (which correctly return `[]`)

## Testing
1. Add unit test in `tests/test_orgql.py`: `test_parse_org_ql_result_none()`
   - Should verify that `parse_org_ql_result(None)` returns `[]`
2. Add unit test: `test_parse_org_ql_result_empty_string()`  
   - Should verify that `parse_org_ql_result('')` returns `[]`
3. Verify all existing tests still pass: `pytest tests/test_orgql.py`
4. Manual verification:
   ```bash
   org-warrior list state:NONEXISTENT --format json
   ```
   Should return `[]` without error

## Expected File Changes
- `org_warrior/Org.py`: Modify `parse_org_ql_result` function (add None check at start)
- `tests/test_orgql.py`: Add two new test cases

## Test Your Work / Show Your Work
Before finishing, run `org-demo --help` and use org-demo to create a demo document `demo-task-golf-golf-mike.org` inside `~/ToReviewByJuan/` describing:
- The bug that was fixed
- How to reproduce the original error
- How the fix prevents it
- Test results showing it works now
