;; Get task by Org ID
;; Parameters:
;;   {org_id} - The Org ID to look up (already escaped)
;;   {files} - Space-separated quoted file paths

(progn
  (require 'org)
  (require 'org-id)
  (let ((org-agenda-files '({files})))
    (let ((m (org-id-find "{org_id}" t)))
      (if (not m)
          ""
        (with-current-buffer (marker-buffer m)
          (save-excursion
            (goto-char m)
            (format "%S"
                    (list (substring-no-properties (org-get-heading t t t t))
                          (org-entry-get nil "TODO")
                          (org-entry-get nil "PRIORITY")
                          (org-entry-get nil "DEADLINE")
                          (org-entry-get nil "SCHEDULED")
                          (buffer-file-name)
                          (line-number-at-pos)
                          (org-entry-get nil "ID")
                          (org-get-tags)))))))))
