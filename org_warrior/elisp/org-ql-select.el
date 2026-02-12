;; Run org-ql query and return results
;; Parameters:
;;   {files} - Space-separated quoted file paths
;;   {query} - org-ql query expression

(progn
  (require 'org-ql)
  (let ((results (org-ql-select '({files})
                   '{query}
                   :action '(list (substring-no-properties (org-get-heading t t t t))
                                  (org-entry-get nil "TODO")
                                  (org-entry-get nil "PRIORITY")
                                  (org-entry-get nil "DEADLINE")
                                  (org-entry-get nil "SCHEDULED")
                                  (buffer-file-name)
                                  (line-number-at-pos)
                                  (org-entry-get nil "ID")
                                  (org-get-tags)))))
    (mapconcat (lambda (r)
                 (if (listp r)
                     (format "%S" r)
                   (format "%s" r)))
               results
               "\n")))
