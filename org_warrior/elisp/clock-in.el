;; Clock in to a task by Org ID
(progn
  (require 'org)
  (require 'org-id)
  (let ((m (org-id-find "{org_id}" t)))
    (if (not m)
        "NOT_FOUND"
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char m)
          (condition-case err
              (progn
                (org-clock-in)
                (save-buffer)
                (substring-no-properties (org-get-heading t t t t)))
            (error (format "ERROR: %s" (error-message-string err)))))))))
