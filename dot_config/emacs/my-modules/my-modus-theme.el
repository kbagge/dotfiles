;; For packaged versions which must use `require'.
(use-package modus-themes
  :ensure t
  :init
  (setq custom-safe-themes t)
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-mixed-fonts t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

   ;;;; Custom set faces - maybe seperate them out, if I do more customization.
  (set-face-attribute 'link nil
                    :weight 'regular)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tinted)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  )
(provide 'my-modus-theme)
