;; Get task body by Org ID
;; Parameters:
;;   {org_id} - The Org ID to look up (already escaped)

(progn
  (require 'org)
  (require 'org-id)
  (let ((m (org-id-find "{org_id}" t)))
    (if (not m)
        "NOT_FOUND"
      (with-current-buffer (marker-buffer m)
        (save-excursion
          (goto-char m)
          (let ((element (org-element-at-point)))
            (buffer-substring-no-properties
             (org-element-property :contents-begin element)
             (or (org-element-property :contents-end element)
                 (org-element-property :end element)))))))))
