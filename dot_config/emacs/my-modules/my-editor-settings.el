;;;; Typo? and line wrapping
(setq-default fill-column 80                          ; Default line width 
              sentence-end-double-space nil           ; Use a single space after dots
                                        ;                bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")           ; Nicer ellipsis

;; This snippet has something to do with where the underline is places. I trust Nicholas from Nano, check out the variable documentation.
(setq x-underline-at-descent-line nil
      x-use-underline-position-properties t
      underline-minimum-offset 10)

;;;; Jump
  ;;;;; Avy
  (use-package avy
    :bind ("M-g a" . avy-goto-char))
;;;; I-menu
;; I should check out how to customize this further
(use-package imenu-list
  :bind (:map my-open-map
              ("i" . imenu-list))
  :init
  (setq-default imenu-list-position 'left
                imenu-max-item-length 1000)
  )

  
  ;;;;; Consult
  ;; Example configuration for Consult
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings in `mode-specific-map'
           ("C-c M-x" . consult-mode-command)
           ("C-c h" . consult-history)
           ("C-c k" . consult-kmacro)
           ("C-c m" . consult-man)
           ("C-c i" . consult-info)
           ([remap Info-search] . consult-info)
           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
           ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find)                  ;; Alternative: consult-fd
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
           ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history)                 ;; orig. next-matching-history-element
           ("M-r" . consult-history))                ;; orig. previous-matching-history-element

    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key "M-.")
    ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-file-register
     consult--source-recent-file consult--source-project-recent-file
     ;; :preview-key "M-."
     :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; "C-+"

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
      ;;;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
      ;;;; 2. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
      ;;;; 3. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
      ;;;; 4. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
      ;;;; 5. No project support
    ;; (setq consult-project-function nil)
    )

;;;; Text edit
;; If I select something and C-y yank or start typing, then just delete the selected text.
(delete-selection-mode 1)
;;;; Expand region
(use-package expand-region
  :bind (("C-+" . er/expand-region)
         ("C-?" . er/contract-region)
         ))      

;;;; Move text
;; Is part of the standard Emacs package, but needs to be activated. Once activated will move selected text or the current line up/down with M-<up> M-<down>.
(use-package move-text
  :config
  (move-text-default-bindings)
  )

;;;; Multiple cursors
;; TODO maybe set some more bindings one day.
(use-package multiple-cursors
  :bind (;("H-SPC" . set-rectangular-region-anchor)
         ;;("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
         ))

;;;; Electric pair mode
(electric-pair-mode 1)

  
  ;;;; Outline mode
  ;; Built in package, that works quite nice. Maybe some day check out ts-fold based on tree-sitter: https://github.com/emacs-tree-sitter/ts-fold?tab=readme-ov-file#ts-fold
  ;; This reddit thread have more inspiration for further keybindings.
  (use-package outline
    :hook (prog-mode . outline-minor-mode)
    :config
    (setq outline-minor-mode-use-buttons t)
    (setq outline-minor-mode-highlight nil) ;; Check this out.
    :bind-keymap ("C-c C-o" . outline-mode-prefix-map)
    ;; :bind (:map outline-minor-mode-map
    ;;             ([C-tab] . bicycle-cycle)
    ;;             ("<backtab>" . bicycle-cycle-global))
    )

  ;; For now I have converted to outli - I think I like it better.
  (use-package outli
    :vc (:fetcher github
                  :repo "jdtsmith/outli")
    :hook ((prog-mode text-mode) . outli-mode))

  (use-package ts-fold
      :vc (:fetcher github
                  :repo "emacs-tree-sitter/ts-fold"))
(provide 'my-editor-settings)
