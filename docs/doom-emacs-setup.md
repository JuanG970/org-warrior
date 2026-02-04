# Doom Emacs Setup for org-warrior Handles

## Quick Setup

Add this to your `~/.doom.d/config.el`:

```elisp
;; Configure org-id for org-warrior handles
(after! org
  (setq org-id-method 'uuid
        org-id-locations-file "~/.org-id-locations"
        org-id-track-globally t)
  
  ;; Auto-create IDs for new headings
  (defun +org-warrior/ensure-id ()
    "Ensure the current heading has an ID property."
    (when (and (eq major-mode 'org-mode)
               (org-at-heading-p)
               (not (org-entry-get nil "ID")))
      (org-id-get-create)))
  
  (add-hook 'org-insert-heading-hook #'+org-warrior/ensure-id))
```

Then run `doom sync` and restart Emacs.

## Manual ID Generation Commands

### For Current Buffer
1. **Generate IDs for all headings in current buffer:**
   - `SPC m i b` (in org-mode)
   - Or: `M-x +org-warrior/generate-buffer-ids`

2. **Create ID for current heading only:**
   - `SPC m i c` (in org-mode)  
   - Or: `M-x org-id-get-create`

### For All Org Files
3. **Update org-id database:**
   - `SPC m i u`
   - Or: `M-x org-id-update-id-locations`

## Verification

After setup, test that it's working:

1. Create a new heading: `M-RET` or `SPC m h i`
2. Check if it got an ID: `C-c C-x p` to show properties
3. You should see a `:ID:` property with a UUID

## org-warrior Integration

Once IDs are created, run:

```bash
org-warrior handles-assign --no-create  # Scan and cache handles
org-warrior list                        # Should show handles
org-warrior show paper-salmon-quiet     # Use handle to show task
```

## Troubleshooting

If org-id creation hangs in batch operations:

1. **Check org-id configuration:**
   ```elisp
   ;; Add to config.el
   (setq org-id-uuid-program "uuidgen")  ; On macOS
   ```

2. **Manual fallback:**
   - Open org file in Emacs
   - Go to each heading without ID
   - Run: `M-x org-id-get-create`

3. **Verify setup:**
   ```elisp
   ;; Evaluate in Emacs to test
   (org-id-get-create)  ; Should return UUID string
   ```

## Advanced: Batch ID Creation Script

For large org collections, create a script:

```elisp
;; Save as ~/bin/org-add-ids.el
(require 'org)
(require 'org-id)

(defun batch-add-org-ids ()
  "Add org-ids to all headings in all agenda files."
  (dolist (file (org-agenda-files))
    (with-current-buffer (find-file-noselect file)
      (org-map-entries 
       (lambda () 
         (unless (org-entry-get nil "ID")
           (org-id-get-create)))
       nil 'file)
      (save-buffer))))

(batch-add-org-ids)
```

Run with: `emacs --batch -l ~/bin/org-add-ids.el`