;; Set or remove a property on a task by Org ID
;; Parameters:
;;   {org_id}  - The Org ID to look up (already escaped)
;;   {action}  - "set" or "remove"
;;   {key}     - Property name (already escaped)
;;   {value}   - Property value (already escaped, ignored for remove)
;;   {files}   - Space-separated quoted file paths

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
                (let ((action "{action}")
                      (key "{key}")
                      (value "{value}"))
                  (cond
                   ((string= action "set")
                    (org-set-property key value))
                   ((string= action "remove")
                    (org-delete-property key)))
                  (save-buffer)
                  (format "%s %s %s"
                          (if (string= action "set")
                              (format "Set %s=%s" key value)
                            (format "Removed %s" key))
                          "on"
                          (substring-no-properties (org-get-heading t t t t))))
              (error (format "ERROR: %s" (error-message-string err))))))))))
