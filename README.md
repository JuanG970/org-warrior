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
- **Friendly 3-word names** like `paper-salmon-quiet` instead of UUIDs
- **Deterministic**: Same Org ID always generates the same handle
- **Only works with tasks that have Org IDs** (:ID: properties in headings)
- Shown by default in list/today; use `--no-handles` to hide them
- Use `--ids` to show Org IDs alongside tasks
- Support prefixes: `paper-salmon` works if unique, warns if ambiguous

Important: ID Creation vs Handle Usage
-------------------------------------
**org-warrior does NOT create Org IDs** - it only uses existing ones.

**ID Creation** (handled by Emacs):
- Add the setup code below to your Emacs config
- IDs are created automatically when you save org files
- Use manual commands in Emacs: `SPC m i b` (Doom) or `M-x org-id-get-create`

**Handle Usage** (handled by org-warrior):
- `handles-assign`: Scan for existing IDs and cache handles
- `show/schedule/deadline/set-state`: Accept handle prefixes or full Org IDs

Commands
--------
- **handles-assign**: Scan org files for existing IDs and populate handle cache (warns about missing IDs)
- **show/schedule/deadline/set-state**: Accept handle prefixes or full Org IDs

Data Files
----------
- Wordlist: src/wordlist.txt (override with ~/.org-warrior/wordlist.txt)
- Cache: ~/.org-warrior/handles.json (auto-created)

Setup (Required for Handles)
----------------------------
Add this to your Emacs config to create IDs automatically:

**Doom Emacs** (~/.doom.d/config.el):
```elisp
(defun org-warrior/ensure-ids-on-save ()
  "Ensure all headings have IDs before saving."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (unless (org-id-get)
           (org-id-get-create)))))))

(add-hook 'before-save-hook 'org-warrior/ensure-ids-on-save)
```

**Other Emacs setups**: See docs/emacs-setup.el for comprehensive setup.

Workflow Examples  
-----------------
```bash
# First time: scan existing IDs
org-warrior handles-assign
# Found 46 IDs out of 991 headings, ⚠️ 945 headings missing IDs

# List tasks (shows handles by default)  
org-warrior list
# 1 paper-salmon-quiet [TODO] Fix parser bug

# Use handles for task operations
org-warrior show paper-salmon-quiet
org-warrior schedule "tomorrow" paper-salmon  
org-warrior set-state paper-salmon-quiet DONE

# Different display options
org-warrior list --ids        # Show Org IDs alongside
org-warrior list --no-handles # Hide handles, show just TODO states
```

More Examples
-------------
    org-warrior list
    org-warrior today --ids
    org-warrior +work due:week
    org-warrior show paper-salmon-quiet
    org-warrior handles-assign
