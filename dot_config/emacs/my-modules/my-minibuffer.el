;;;; Minibuffer
;;;;; Vertico
;; Enable vertico - Shows xx possible lines in minibuffer.
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-cycle t
        vertico-count-format nil ; No prefix with number of entries
        vertico-count 7) ; Number of occurences to show in the minibuffer.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy) ; Makes it more tidy when using ~/ completion in find-file.
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))
;;;;; Marginalia 
;; Enable rich annotations using the Marginalia package in minibuffer.
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(provide 'my-minibuffer)
