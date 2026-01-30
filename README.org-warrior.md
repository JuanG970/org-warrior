org-warrior (draft)

Overview
- `src/org-warrior`: TaskWarrior-like CLI using **org-ql** as a backend.
  - Executes org-ql queries via an existing Emacs daemon (`emacsclient --socket-name edit`).
  - Intended to be org-file centric: read/query tasks quickly; write operations go through Emacs/Org for correctness (IDs, scheduling, state changes).

Usage
- `./src/org-warrior --help`

Notes
- Uses `org-id` for stable task identifiers.
- Requires a running Emacs server (`edit`).
