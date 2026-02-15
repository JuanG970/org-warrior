# Fix: org-warrior add-child Ordering (Task alpha-lima-golf)

## Problem
The `org-warrior add-child` command was inserting new children at the TOP of the parent's subtree (reverse-chronological order, newest first), but the requirement was to insert them at the BOTTOM (chronological order, oldest first).

## Solution
Modified `org_warrior/elisp/add-child-task.el` to:
1. After skipping past the parent's properties drawer
2. Add a `while` loop that skips forward past ALL existing child headings using `org-end-of-subtree`
3. Insert the new child at the END (after all existing children)

### Key Changes in add-child-task.el:
```elisp
;; OLD: Insert immediately after properties (top)
(forward-line 1)
(beginning-of-line)
(insert ...)

;; NEW: Skip past all children, then insert at end (bottom)
(forward-line 1)
(beginning-of-line)
;; Skip forward past all child headings
(while (and (not (eobp))
            (looking-at-p "^\\*+")
            (> (org-current-level) parent-level))
  (org-end-of-subtree t t))
;; Now we're at the end of all children - insert here
(beginning-of-line)
(insert ...)
```

## Expected Behavior
Given a parent task, adding children "A", "B", "C" in that order produces:

```org
* Parent
:PROPERTIES:
:ID: parent-id
:END:
** TODO Child A     <- Oldest (added first)
** TODO Child B
** TODO Child C     <- Newest (added last, at bottom)
```

## Tests Updated
- Updated `tests/test_add_child_integration.py` to verify chronological ordering
- Changed test descriptions from "reverse-chronological (newest first)" to "chronological (oldest first, newest at bottom)"
- All 17 tests pass

## Verification
```bash
cd /Users/juangonzalez/PersonalProjects/org-warrior
python -m pytest tests/test_add_child.py tests/test_add_child_integration.py -v
# Result: 17 passed
```

## Files Modified
1. `org_warrior/elisp/add-child-task.el` - Core insertion logic
2. `tests/test_add_child_integration.py` - Integration tests and documentation
