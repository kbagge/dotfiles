;;; Denote
(unless (package-installed-p 'denote)
  (package-install 'denote))

;; Denote
(use-package denote
  :init
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (denote-rename-buffer-mode t)
  (setq denote-journal-extras-title-format 'day-date-month-year)
  :custom
  (denote-directory "~/org/")

  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :bind (:map my-notes-map
              ("n" . denote-create-note)
              ("j" . denote-journal-extras-new-or-existing-entry)
              ("i" . denote-link-or-create)
              ("f" . denote-open-or-create)
              ("l" . denote-find-link)
              ("b" . denote-find-backlink)
              ("D" . denote-org-dblock-insert-links)
              ("r" . denote-rename-file-using-front-matter)
              ("R" . denote-rename-file)
              ("s" . denote-subdirectory)
              ("S" . my-denote-region-subdirectory)
              ("k" . denote-keywords-add)
              ("K" . denote-keywords-remove))
)

(defun my/denote-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep nil) ;; Set this to nil to delete the original!
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (denote-journal-extras-new-or-existing-entry)
      (setq today-file (buffer-file-name))
      (setq pos (point)))

    ;; Only refile if the target file is different than the current file
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks:" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
             (lambda ()
               (when (equal org-state "DONE")
                 (my/denote-copy-todo-to-today))))

(use-package consult-denote
  :init
  (consult-denote-mode 1)
  )

(provide 'my-denote-setup)
