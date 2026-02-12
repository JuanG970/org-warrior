;; Add a new TODO task to the inbox
;; Parameters:
;;   {title} - Task title (already escaped)
;;   {file} - Inbox file path
;;   {heading} - Inbox heading (already escaped)

(progn
  (require 'org)
  (require 'org-id)
  (let* ((file (expand-file-name "{file}"))
         (heading "{heading}"))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward (format "^\\*+ %s\\b" (regexp-quote heading)) nil t)
          (goto-char (point-max))
          (insert (format "* %s\n" heading)))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (let ((pos (point)))
          (insert (format "** TODO %s\n" "{title}"))
          (goto-char pos)
          (org-id-get-create)
          (save-buffer)
          (org-id-get))))))
