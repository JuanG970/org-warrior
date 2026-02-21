;; Buffer cleanup wrapper for org-warrior elisp operations
;; Provides a macro that tracks which org buffers are open before an operation
;; and kills any new unmodified org buffers opened during the operation.

(defmacro org-warrior-with-buffer-cleanup (&rest body)
  "Execute BODY and kill any new unmodified org buffers opened during execution.
Buffers that existed before the operation, or that are modified, are preserved."
  (let ((initial-buffers-var (make-symbol "initial-buffers"))
        (result-var (make-symbol "result")))
    `(let ((,initial-buffers-var (buffer-list)))
       (let ((,result-var (progn ,@body)))
         (dolist (buf (buffer-list))
           (when (and (not (memq buf ,initial-buffers-var))
                      (buffer-file-name buf)
                      (string-suffix-p ".org" (buffer-file-name buf))
                      (not (buffer-modified-p buf)))
             (kill-buffer buf)))
         ,result-var))))

(provide 'buffer-cleanup-wrapper)
