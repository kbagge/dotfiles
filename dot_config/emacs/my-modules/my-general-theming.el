;; Highlight current line
(global-hl-line-mode 1)

;;;; Solaire mode
;; Make it easier to differentiate real buffers from less important.
(use-package solaire-mode
  :init
  (solaire-global-mode +1))

;;;; Helpfull
(use-package helpful
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  :bind (:map help-map
              ("f" . helpful-callable)
              ("F" . helpful-function)
              ("C" . helpful-command)
              ("v" . helpful-variable)
              ("k" . helpful-key)
              ("C-p" . helpful-at-point))    ;; Lookup the current symbol at point. C-c C-d is a common keybinding, but I prefer C-h C-p, which was bound to known bugs in emacs.
  )
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.

;; Look up *C*ommands.
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.

;;;; Rainbow delimeters
(use-package rainbow-delimiters
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;;;; Which key
(use-package which-key
  :defer 1
  :init
  (setq which-key-show-early-on-C-h t)
  (setq-local which-key-idle-delay 0.3 which-key-idle-secondary-delay 0.05)
  :custom
  (which-key-mode 1))

;;;; Spacious padding
(require 'spacious-padding)

;; These is the default value, but I keep it here for visiibility.
(setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))

;; Read the doc string of `spacious-padding-subtle-mode-line' as it
;; is very flexible and provides several examples.
(setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))

(spacious-padding-mode 1)
(provide 'my-general-theming)
