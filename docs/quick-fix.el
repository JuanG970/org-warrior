;; Quick fix for current session
;; Evaluate this in your Emacs to add IDs to current buffer

(defun org-warrior-fix-current-buffer ()
  "Add org-ids to all headings in current buffer that don't have them."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "This command only works in org-mode buffers")
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (unless (org-entry-get nil "ID")
           (org-id-get-create)
           (setq count (1+ count)))))
      (when (> count 0)
        (save-buffer)
        (message "✓ Created %d org-ids in %s" count (buffer-name)))
      (when (= count 0)
        (message "✓ All headings in %s already have org-ids" (buffer-name))))))

;; Run this to fix current buffer:
;; M-x org-warrior-fix-current-buffer
