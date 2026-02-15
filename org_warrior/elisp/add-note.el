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
                     (parent-level (org-current-level))
                     (entry-end (save-excursion
                                  (org-end-of-subtree t t)
                                  (point))))
                ;; Move to end of meta-data (planning, drawers) within the entry
                (org-end-of-meta-data t)
                ;; Skip forward past existing notes and their content to append at the end
                ;; A note starts with "- Note taken on" at column 0
                ;; After each note header, there may be indented continuation lines
                (while (and (< (point) entry-end)
                            (not (looking-at "^\\*")))  ; stop at next heading
                  (cond
                   ;; Note header line - skip it and any following indented lines
                   ((looking-at "^- Note taken on")
                    (forward-line 1)
                    ;; Skip indented continuation lines (body of the note)
                    (while (and (< (point) entry-end)
                                (looking-at "^[ \t]+[^ \t\n]"))
                      (forward-line 1)))
                   ;; Empty line - skip it
                   ((looking-at "^[ \t]*$")
                    (forward-line 1))
                   ;; Any other content (not a note) - stop here
                   (t
                    (goto-char entry-end))))
                ;; Now we're at the right position to insert
                (beginning-of-line)
                ;; Insert the note in chronological order (at the bottom)
                (insert (format "- Note taken on [%s]\n  %s\n"
                                (format-time-string "%Y-%m-%d %a %H:%M")
                                "{note}"))
                (save-buffer)
                (format "Note added to: %s" heading))
            (error (format "ERROR: %s" (error-message-string err)))))))))
