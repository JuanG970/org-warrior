;; org-warrior Emacs setup
;; 
;; Add this to your Emacs configuration to ensure all new Org headings
;; automatically get Org IDs, making them compatible with org-warrior handles.

;; Automatically assign Org IDs to new headings
(defun org-warrior-ensure-id ()
  "Ensure the current heading has an ID property."
  (when (and (eq major-mode 'org-mode)
             (org-at-heading-p)
             (not (org-entry-get nil "ID")))
    (org-id-get-create)))

;; Hook to auto-create IDs when creating new headings
(add-hook 'org-insert-heading-hook #'org-warrior-ensure-id)

;; Also create ID when promoting/demoting (in case of structure changes)
(add-hook 'org-after-promote-entry-hook #'org-warrior-ensure-id)
(add-hook 'org-after-demote-entry-hook #'org-warrior-ensure-id)

;; Alternative: Use org-id-link-to-org-use-id to auto-create IDs
;; This creates IDs when creating links to headings
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)