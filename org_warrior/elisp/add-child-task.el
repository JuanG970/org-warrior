;; Add a child TODO task under a parent heading found by Org ID
;; Parameters:
;;   {parent_org_id} - The Org ID of the parent task (already escaped)
;;   {title}         - Child task title (already escaped)
;;   {files}         - Space-separated quoted file paths

(progn
  (require 'org)
  (require 'org-id)
  (let ((org-agenda-files '({files})))
    (let ((m (org-id-find "{parent_org_id}" t)))
      (if (not m)
          "NOT_FOUND"
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char m)
            (condition-case err
                (let ((child-stars (make-string (1+ (org-current-level)) ?*)))
                  ;; Move to end of parent's subtree (handles properties and all children)
                  (org-end-of-subtree t t)
                  (unless (bolp) (insert "\n"))
                  ;; Insert heading then use org-todo to set state via the org API
                  (insert (concat child-stars " {title}\n"))
                  (forward-line -1)
                  (beginning-of-line)
                  (org-todo "TODO")
                  ;; Create org-id for the new heading
                  (let ((new-id (org-id-get-create)))
                    (save-buffer)
                    new-id))
              (error (format "ERROR: %s" (error-message-string err))))))))))
