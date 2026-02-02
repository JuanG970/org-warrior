# org-warrior Development Guide

## Overview

org-warrior is a TaskWarrior-like CLI that uses **org-ql** as its backend. It operates by executing org-ql queries through an existing Emacs daemon (`emacsclient --socket-name edit`).

**Architecture principle**: Read/query operations are fast and go directly through org-ql. Write operations (scheduling, state changes, etc.) go through Emacs/Org for correctness, ensuring proper handling of org-id, scheduling, and state transitions.

## Build & Test

### Build
```bash
./scripts/build.sh
# Installs to ~/cli-apps/scripts/org-warrior
```

### Test
```bash
# Run full integration test suite
./tests/test_org_warrior.sh
```

The test suite:
- Creates a temporary org file structure with realistic test data
- Uses dynamic dates to ensure reproducible relative date tests
- Tests all major commands and filters
- Validates task counts and content matching

## Key Architecture Patterns

### Emacs Integration
- **Requires a running Emacs daemon** with the socket name configured in `ORG_WARRIOR_SERVER` (default: `edit`)
- All operations use `emacsclient` to communicate with the daemon
- Two execution modes:
  - **Queries**: Eval elisp that returns formatted strings (fast, read-only)
  - **Modifications**: Eval elisp that manipulates buffers and saves files (uses proper Org APIs)

### Org File Resolution
- `resolve_org_files()` recursively walks directories to find all `.org` files
- Supports `ORG_WARRIOR_FILES` as a path-separated list of files/directories
- Deduplicates results while preserving first-seen order

### Load Path Auto-Detection
- `get_doom_load_paths()` auto-discovers Doom Emacs straight packages
- Required packages: org-ql, org, dash, s, ts, peg, ov, org-super-agenda, compat, f, ht, map
- Finds the most recent build directory (e.g., `build-30.2`)
- Falls back to custom `ORG_WARRIOR_LOAD_PATH` if set

### Query Building
- `build_query()` translates TaskWarrior-style filters to org-ql syntax
  - `+tag` → `(tags "tag")`
  - `pri:A` → `(priority "A")`
  - `due:today` → `(deadline :on today)`
  - `project:` → `(ancestors (todo "PROJECT"))`
  - Text → `(regexp "text")`
- Default query: `(and (todo) (not (done)))`

### Result Parsing
- `parse_result()` handles two formats:
  - Simple: plain heading text
  - Full: `("heading" "TODO" "A" deadline scheduled "filepath" lineno)`
- Priority "B" is treated as no priority (Org default)
- Handles `nil` values from elisp for optional fields

### Git Auto-Commit
- `git_commit_org()` automatically commits changes to `~/org` if it's a git repo
- Only commits when there are actual changes (`git status --porcelain`)
- Called after write operations (add, set-state, schedule)

## Environment Variables

- `ORG_WARRIOR_FILES`: Path(s) to org files/directories (default: `~/org`)
- `ORG_WARRIOR_SERVER`: Emacs server socket name (default: `edit`)
- `ORG_WARRIOR_LOAD_PATH`: Custom load paths (colon-separated)
- `EMACS_CMD`: Emacs client command (default: `emacsclient`)

## Task Identification

Uses **org-id** for stable task references:
- `show <ORG-ID>`: Fast lookup via property query `(property "ID" "...")`
- `show index <n>`: Fallback index-based lookup within a filter context
- Write commands require org-id and use `org-id-find` for precise targeting

## Command Patterns

### Read Commands (Fast)
Direct org-ql queries with formatted output:
- `list`, `next`, `today`, `week`, `overdue`, `done`, `projects`, `stuck`, `waiting`, `tag`, `priority`, `search`, `query`

### Write Commands (Safe)
Evaluate elisp that manipulates Org buffers properly:
- `add`: Creates TODO under "Inbox" heading in `~/org/inbox.org` with auto-generated org-id
- `set-state <ID> <STATE>`: Uses `org-todo` to handle state transitions
- `schedule <ID> <DATE>`: Uses `org-schedule` with `org-read-date` for proper date parsing
- `start <filter> <id>`: Uses `org-clock-in` after navigating to task location

### Hybrid Commands
- `jira-sync`: Triggers `org-jira-get-issues` (hits external service)

## Conventions

### Error Handling
- Stderr for error messages
- Filter out Loading messages and Warnings from emacsclient stderr
- Check for NOT_FOUND sentinel in elisp responses for ID lookups
- Return non-zero exit codes on failures

### Output Formatting
- Task list: `idx [TODO] (A) heading DUE:date` 
- Footer: `N task(s)`
- Detail view: Multi-line with Location showing `filepath:lineno`
- Empty results: Contextual messages like "No tasks for today."

### Date Formats
- Input: `YYYY-MM-DD`, `+Nd`, `today`, `week`
- Output: Org timestamp format `<YYYY-MM-DD Day>`
- Relative dates via `org-read-date`

### String Escaping
- Escape backslashes and quotes when building elisp strings
- Emacsclient returns quoted strings: strip outer quotes and unescape `\n`, `\"`
