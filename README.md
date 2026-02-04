org-warrior
==========

Overview
--------
org-warrior is a TaskWarrior-like CLI that queries Org files via org-ql and an Emacs daemon.
Read operations use org-ql; write operations go through Emacs/Org for correct IDs and state changes.

Requirements
------------
- Running Emacs daemon (default socket: edit)
- org-ql available in Emacs

Usage
-----
    ./src/org-warrior --help

Handles
-------
- Org IDs remain canonical.
- Friendly handles are deterministic and derived from the Org ID.
- Handles are shown by default in list/today; use --no-handles to hide them.
- Use --ids to show Org IDs alongside tasks.

Commands
--------
- handles-assign: ensure all tasks have Org IDs and populate the handle cache.
- show/schedule/deadline/set-state accept handle prefixes or full Org IDs.

Data Files
----------
- Wordlist: src/wordlist.txt (override with ~/.org-warrior/wordlist.txt).
- Cache: ~/.org-warrior/handles.json (auto-created).

Setup
-----
For automatic Org ID creation in Emacs:
- **Doom Emacs users:** See docs/doom-emacs-setup.md
- **Other Emacs users:** Add docs/emacs-setup.el to your config
- **Quick fix for existing files:** Load docs/quick-fix.el

Examples
--------
    org-warrior list
    org-warrior list --ids
    org-warrior list --no-handles
    org-warrior show paper-salmon-quiet
    org-warrior handles-assign
