;; Run org-ql query and return results
;; Parameters:
;;   {files} - Space-separated quoted file paths
;;   {query} - org-ql query expression

(progn
  (require 'org-ql)
  (require 'json)
  (let ((org-agenda-files '({files})))
    (condition-case err
        (let (
              (results (org-ql-select
                         '({files})
                         '{query}
                         :action (lambda ()
                                   `(
                                     (heading . ,(substring-no-properties (org-get-heading t t t t)))
                                     (todo . ,(org-entry-get nil "TODO"))
                                     (priority . ,(org-entry-get nil "PRIORITY"))
                                     (deadline . ,(org-entry-get nil "DEADLINE"))
                                     (scheduled . ,(org-entry-get nil "SCHEDULED"))
                                     (filename . ,(buffer-file-name))
                                     (linenumber . ,(line-number-at-pos))
                                     (id . ,(org-entry-get nil "ID"))
                                     (tags . ,(org-get-tags))
                                     (properties . ,(org-entry-properties))
                                     )
                                   ))
                       ))
          (json-encode results))
      (error (format "ERROR: %s" (error-message-string err))))))
