;; Set deadline for a task by Org ID
;; Parameters:
;;   {org_id} - The Org ID to look up (already escaped)
;;   {date}   - The deadline date
;;   {files}  - Space-separated quoted file paths

(progn
  (require 'org)
  (require 'org-id)
  (let ((org-agenda-files '({files})))
    (let ((m (org-id-find "{org_id}" t)))
      (if (not m)
          "NOT_FOUND"
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char m)
            (condition-case err
                (progn
                  (org-deadline nil "{date}")
                  (save-buffer)
                  (substring-no-properties (org-get-heading t t t t)))
              (error (format "ERROR: %s" (error-message-string err))))))))))
