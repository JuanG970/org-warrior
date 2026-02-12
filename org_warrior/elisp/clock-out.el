;; Clock out from current task
(progn
  (require 'org-clock)
  (condition-case err
      (progn
        (org-clock-out)
        (let ((buf (marker-buffer org-clock-marker)))
          (when buf
            (with-current-buffer buf
              (save-buffer))))
        "SUCCESS")
    (error (format "ERROR: %s" (error-message-string err)))))
