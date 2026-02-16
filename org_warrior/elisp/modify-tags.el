;; Modify tags on a task by Org ID
;; Parameters:
;;   {org_id}  - The Org ID to look up (already escaped)
;;   {action}  - "add" or "remove"
;;   {tag}     - Tag name (already escaped)
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
                (let* ((action "{action}")
                       (tag "{tag}")
                       (current-tags (org-get-tags nil t)))
                  (cond
                   ((string= action "add")
                    (unless (member tag current-tags)
                      (org-set-tags (append current-tags (list tag)))))
                   ((string= action "remove")
                    (org-set-tags (remove tag current-tags))))
                  (save-buffer)
                  (format "%s %s %s"
                          (if (string= action "add") "Added" "Removed")
                          tag
                          (substring-no-properties (org-get-heading t t t t))))
              (error (format "ERROR: %s" (error-message-string err))))))))))
