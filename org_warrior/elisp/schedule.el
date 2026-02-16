;; Schedule a task by Org ID with a date
;; Parameters:
;;   {org_id} - The Org ID to look up (already escaped)
;;   {date}   - The date to schedule
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
                  (org-schedule nil "{date}")
                  (save-buffer)
                  (substring-no-properties (org-get-heading t t t t)))
              (error (format "ERROR: %s" (error-message-string err))))))))))
