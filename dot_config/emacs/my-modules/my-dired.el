;;;; Dired / dirvish setup
  (use-package dired
    :ensure nil
    :init
    (setq dired-recursive-copies 'always) ; operate on entire folder
    (setq dired-recursive-delete 'always) ; operate on entire folder
    (setq dired-dwim-target t) ; Let emacs try to get the destination (other dired window)

    :bind (:map my-open-map
                ("d" . dired))
    :hook
    (dired-mode . dired-hide-details-mode)
    )
(provide 'my-dired)
