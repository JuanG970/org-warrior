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
              (let ((heading (substring-no-properties (org-get-heading t t t t))))
                ;; Move to end of meta-data (planning, drawers) within the entry
                (org-end-of-meta-data t)
                ;; Insert timestamped note
                (insert (format "\n- Note taken on [%s] \\\\\n  %s\n"
                                (format-time-string "%Y-%m-%d %a %H:%M")
                                "{note}"))
                (save-buffer)
                (format "Note added to: %s" heading))
            (error (format "ERROR: %s" (error-message-string err)))))))))
