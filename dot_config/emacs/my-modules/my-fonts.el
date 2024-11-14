;;;; Fonts
  ;; I use fontaine to set my fonts
  (use-package fontaine
    :init
    (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
    :config
    (setq fontaine-presets
          '((small
             ;; :default-family "Iosevka Comfy Fixed"
             :default-height 100
             :fixed-pitch-height 90
             :variable-pitch-height 110)
            (regular
             :fixed-pitch-height 110
             :variable-pitch-height 145)
            (medium
             :fixed-pitch-height 105
             :variable-pitch-height 140)
            (large
             :fixed-pitch-height 115
             :variable-pitch-weight semilight
             :variable-pitch-height 160
             :bold-weight extrabold)
            (presentation
             :fixed-pitch-height 135
             :variable-pitch-weight semilight
             :variable-pitch-height 170
             :bold-weight extrabold)
            (jumbo
             :default-weight semilight
             :default-height 220
             :bold-weight extrabold)
            (t
             ;; I keep all properties for didactic purposes, but most can be
             ;; omitted.  See the fontaine manual for the technicalities:
             ;; <https://protesilaos.com/emacs/fontaine>.
             ;;           :default-family "IBM Plex Mono"
             :default-family "Overpass Nerd Font Propo Regular"
             :default-weight light
             :default-height 120
             :fixed-pitch-family "Overpass Nerd Font Propo Regular" ; nil - means falls back to :default-family
             :fixed-pitch-weight semilight ; falls back to :default-weight
             :fixed-pitch-height 110
             :fixed-pitch-serif-family nil ; falls back to :default-family
             :fixed-pitch-serif-weight nil ; falls back to :default-weight
             :fixed-pitch-serif-height 1.0
             ;;           :variable-pitch-family "IBM Plex Sans Condensed"
             :variable-pitch-family "Overpass"
             :variable-pitch-weight semilight
             :variable-pitch-height 155
             :bold-family nil ; use whatever the underlying face has
             :bold-weight regular
             :italic-family nil
             :italic-slant italic
             :line-spacing 1)))
    (fontaine-set-preset 'medium)
    )

  ;;;; Icons
  (use-package nerd-icons
    :ensure t
    :bind (:map my-insert-map
                ("i" . nerd-icons-insert)))

  (use-package nerd-icons-dired
    :ensure t
    :after nerd-icons
    :hook (dired-mode . nerd-icons-dired-mode)
    :config
    (nerd-icons-dired-mode t)
    )

  (use-package nerd-icons-completion
    :after marginalia
    :config
    (nerd-icons-completion-mode)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

  (use-package nerd-icons-ibuffer
    :ensure t
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

  (use-package nerd-icons-corfu
    :after corfu nerd-icons
    :ensure t
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

    ;; Optionally:
    (setq nerd-icons-corfu-mapping
          '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
            (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
            ;; ...
            (t :style "cod" :icon "code" :face font-lock-warning-face))))
  ;; Remember to add an entry for `t', the library uses that as default.
  ;; The Custom interface is also supported for tuning the variable above.)

(provide 'my-fonts)
