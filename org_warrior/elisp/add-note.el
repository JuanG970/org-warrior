;; Add a note to a task by Org ID
;; Parameters:
;;   {org_id} - The Org ID to look up (already escaped)
;;   {note}   - The note text to append (already escaped)

(progn
  (require 'org)
  (require 'org-id)
  (let ((m (org-id-find "{org_id}" t)))
    (if (not m)
        "NOT_FOUND"
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char m)
          (condition-case err
              (let* ((heading (substring-no-properties (org-get-heading t t t t)))
                     (parent-level (org-current-level)))
                ;; Move to end of meta-data (planning, drawers) within the entry
                (org-end-of-meta-data t)
                ;; Skip forward past all existing notes and blank lines
                ;; to insert at the end (chronological order: oldest first, newest at bottom)
                (while (and (not (eobp))
                            (not (looking-at-p "^\\*+"))  ; Stop at child headings
                            (or (looking-at-p "^[ \t]*- Note taken on")
                                (looking-at-p "^[ \t]*$")))  ; Skip notes and blank lines
                  (forward-line 1))
                ;; Now we're either at EOF, a child heading, or content
                ;; Insert note here (at the end of notes, before children)
                (beginning-of-line)
                (insert (format "- Note taken on [%s] \\\\\n  %s\n"
                                (format-time-string "%Y-%m-%d %a %H:%M")
                                "{note}"))
                (save-buffer)
                (format "Note added to: %s" heading))
            (error (format "ERROR: %s" (error-message-string err)))))))))
