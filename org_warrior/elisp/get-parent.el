;; Get the parent heading of a task by Org ID
;; Parameters:
;;   {org_id} - The Org ID of the child task (already escaped)
;;   {files}  - Space-separated quoted file paths
;;
;; Returns:
;;   JSON object with parent task data
;;   "NOT_FOUND" if the task ID doesn't exist
;;   "NO_PARENT" if the task has no parent heading

(progn
  (require 'org)
  (require 'org-id)
  (require 'json)
  (let ((org-agenda-files '({files})))
    (let ((m (org-id-find "{org_id}" t)))
      (if (not m)
          "NOT_FOUND"
        (condition-case err
            (with-current-buffer (marker-buffer m)
              (save-excursion
                (goto-char m)
                (if (org-up-heading-safe)
                    (json-encode
                     `((heading . ,(substring-no-properties (org-get-heading t t t t)))
                       (todo . ,(org-entry-get nil "TODO"))
                       (priority . ,(org-entry-get nil "PRIORITY"))
                       (deadline . ,(org-entry-get nil "DEADLINE"))
                       (scheduled . ,(org-entry-get nil "SCHEDULED"))
                       (filename . ,(buffer-file-name))
                       (linenumber . ,(line-number-at-pos))
                       (id . ,(org-entry-get nil "ID"))
                       (tags . ,(org-get-tags))
                       (properties . ,(org-entry-properties))))
                  "NO_PARENT")))
          (error (format "ERROR: %s" (error-message-string err))))))))
