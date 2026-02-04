;; Doom Emacs Configuration for org-warrior handles
;; Add this to your ~/.doom.d/config.el

;; Configure org-id for automatic ID creation
(after! org
  ;; Set org-id method and locations
  (setq org-id-method 'uuid                    ; Use UUID format
        org-id-locations-file "~/.org-id-locations"  ; Cache file location
        org-id-track-globally t                ; Track IDs across all org files
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Initialize org-id database on startup
  (when (file-exists-p org-directory)
    (org-id-update-id-locations))

  ;; Automatically create IDs for new headings
  (defun +org-warrior/ensure-id ()
    "Ensure the current heading has an ID property."
    (when (and (eq major-mode 'org-mode)
               (org-at-heading-p)
               (not (org-entry-get nil "ID")))
      (org-id-get-create)))

  ;; Hook to auto-create IDs when creating new headings
  (add-hook 'org-insert-heading-hook #'+org-warrior/ensure-id)
  
  ;; Also create ID when promoting/demoting headings
  (add-hook 'org-after-promote-entry-hook #'+org-warrior/ensure-id)
  (add-hook 'org-after-demote-entry-hook #'+org-warrior/ensure-id)

  ;; Optional: Create ID when capturing new tasks
  (add-hook 'org-capture-before-finalize-hook #'+org-warrior/ensure-id))

;; Keybindings for org-id management
(map! :after org
      :map org-mode-map
      :localleader
      (:prefix-map ("i" . "id")
       :desc "Create ID for current heading" "c" #'org-id-get-create
       :desc "Generate IDs for all headings in buffer" "b" #'+org-warrior/generate-buffer-ids
       :desc "Update ID locations database" "u" #'org-id-update-id-locations
       :desc "Find heading by ID" "f" #'org-id-find))

;; Custom function to generate IDs for all headings in current buffer
(defun +org-warrior/generate-buffer-ids ()
  "Generate org-id for all headings in the current buffer that don't have one."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward org-heading-regexp nil t)
          (org-back-to-heading t)
          (unless (org-entry-get nil "ID")
            (org-id-get-create)
            (setq count (1+ count))))
        (when (> count 0)
          (save-buffer)
          (message "Created %d org-ids in current buffer" count))
        (when (= count 0)
          (message "All headings already have org-ids"))))))

;; Optional: Auto-create IDs when saving org files (aggressive mode)
;; Uncomment the next section if you want ALL headings to get IDs automatically

;; (defun +org-warrior/auto-create-ids-on-save ()
;;   "Automatically create org-ids for headings without them when saving."
;;   (when (eq major-mode 'org-mode)
;;     (+org-warrior/generate-buffer-ids)))
;; 
;; (add-hook 'before-save-hook #'+org-warrior/auto-create-ids-on-save)

;; Configure org-capture templates to always create IDs (optional)
;; This ensures captured tasks always get org-ids
(after! org-capture
  (defun +org-warrior/org-capture-create-id ()
    "Create an org-id for the captured entry."
    (org-id-get-create))
  
  (add-hook 'org-capture-before-finalize-hook #'+org-warrior/org-capture-create-id))