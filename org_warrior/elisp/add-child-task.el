;; Add a child TODO task under a parent heading found by Org ID
;; Parameters:
;;   {parent_org_id} - The Org ID of the parent task (already escaped)
;;   {title}         - Child task title (already escaped)

(progn
  (require 'org)
  (require 'org-id)
  (let ((m (org-id-find "{parent_org_id}" t)))
    (if (not m)
        "NOT_FOUND"
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char m)
          (condition-case err
              (let* ((parent-level (org-current-level))
                     (child-stars (make-string (1+ parent-level) ?*))
                     (parent-heading (substring-no-properties (org-get-heading t t t t))))
                ;; Move to end of parent heading line
                (end-of-line)
                ;; Skip past properties drawer if present
                (when (save-excursion
                        (forward-line 1)
                        (looking-at-p "^[ \t]*:PROPERTIES:"))
                  (re-search-forward "^[ \t]*:END:" nil t)
                  (end-of-line))
                ;; Now we're after parent properties. Move to next line.
                (forward-line 1)
                (beginning-of-line)
                (let ((pos (point)))
                  (insert (format "%s TODO %s\n" child-stars "{title}"))
                  (goto-char pos)
                  (let ((new-id (org-id-get-create)))
                    (save-buffer)
                    new-id)))
            (error (format "ERROR: %s" (error-message-string err)))))))))
