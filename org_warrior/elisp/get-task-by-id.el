;; Get task by Org ID
;; Parameters:
;;   {org_id} - The Org ID to look up (already escaped)

(progn
  (require 'org)
  (require 'org-id)
  (require 'org-ql)
  (let* ((query '(property "ID" "{org_id}"))
         (results (org-ql-select (org-id-files)
                    query
                    :action '(list (substring-no-properties (org-get-heading t t t t))
                                   (org-entry-get nil "TODO")
                                   (org-entry-get nil "PRIORITY")
                                   (org-entry-get nil "DEADLINE")
                                   (org-entry-get nil "SCHEDULED")
                                   (buffer-file-name)
                                   (line-number-at-pos)
                                   (org-entry-get nil "ID")
                                   (org-get-tags)))))
    (if results
        (format "%S" (car results))
      "")))
