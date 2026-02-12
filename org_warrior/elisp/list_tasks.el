;;; list_tasks.el Lists tasks based on a org-ql filter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Juan Gonzalez
;;
;; Author: Juan Gonzalez <juang@wolfram.com>
;; Maintainer: Juan Gonzalez <juang@wolfram.com>
;; Created: February 12, 2026
;; Modified: February 12, 2026
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  
;;
;;; Code:

(progn
  (require 'org-ql)
  (let ((results (org-ql-select '({files_quoted})
                   '{query}
                   :action '{select_expr})))
    (mapconcat (lambda (r)
                 (if (listp r)
                     (format "%S" r)
                   (format "%s" r)))
               results
               "\\n")))


;;; list_tasks.el ends here
