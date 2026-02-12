# org-warrior Development Guide

## Overview

org-warrior is a single-file Python CLI (`src/org-warrior`) with zero external dependencies that talks to Emacs via `emacsclient` to query org-ql. It provides a TaskWarrior-like interface for Org mode.

## Build & Test

```bash
# Unit tests
python3 -m pytest tests/ -v

# Integration tests (requires running Emacs daemon)
./tests/test_org_warrior.sh

# Lint
ruff check src/org-warrior tests/
```

## Module Import Pattern

The CLI has no `.py` extension. `spec_from_file_location` returns `None` without
an explicit loader. Use `SourceFileLoader` instead:
```python
import importlib.machinery, importlib.util
_loader = importlib.machinery.SourceFileLoader("org_warrior", SRC_PATH)
_spec = importlib.util.spec_from_loader("org_warrior", _loader, origin=SRC_PATH)
ow = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(ow)
```

## Key Conventions

### Elisp Escaping
Always use `_escape_elisp(s)` when embedding user-supplied strings into elisp f-strings. Never embed raw variables into elisp queries.

### Error Output
- All error messages go to `sys.stderr`
- Command functions return int (0 for success, 1 for failure)
- `main()` calls `sys.exit()` with the return value

### Architecture
- **Read operations**: Direct org-ql queries via `run_org_ql()` — fast, read-only
- **Write operations**: Elisp eval via `emacs_eval()` — uses proper Org APIs
- **Subprocess timeout**: All `emacsclient` calls use `EMACS_TIMEOUT` (15s)

### Task Identification
- **Handles**: Deterministic 3-word names derived from Org IDs (e.g., `paper-salmon-quiet`)
- **Org IDs**: UUID-style `:ID:` properties on headings
- **Numeric indices**: Positional, unstable across operations — prefer handles

## Environment Variables

- `ORG_WARRIOR_FILES`: Path(s) to org files/directories (default: `~/org`)
- `ORG_WARRIOR_SERVER`: Emacs server socket name (default: `edit`)
- `ORG_WARRIOR_LOAD_PATH`: Custom elisp load paths (colon-separated)
- `EMACS_CMD`: Emacs client command (default: `emacsclient`)
