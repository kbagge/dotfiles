;;;; Writeroom mode
;; For better focus
(use-package writeroom-mode
  :defer 3
  :bind
  ("<f8>" . 'writeroom-mode)
  )

  
  ;;;; tabs, projects etc.
  ;;Der er flere indbyggede pakker, som delvist overlapper.
  ;; - desktop.el :: Kan gemme og senere hente window samt tab, og buffer åbne på et bestemt tidspunkt. Kan gøres automatisk når emacs åbnes og lukkes.
  ;; - bookmark :: bookmark a specific file
  ;; - project.el :: project management
  ;; - tab-bar-mode :: Giver en slags tabs, over window layout med buffere
  ;; - tab-line-mode :: Giver en slags tabs, over hvilke buffere som har været åbne i et enkelt vindue
  (use-package tab-bar
    :config
    (setq tab-bar-select-tab-modifiers 't)
    (tab-bar-history-mode 1)
     )

  ;;;; Bookmark plus
  (use-package bookmark+
    :vc (:fetcher github
                  :repo "emacsmirror/bookmark-plus")
    ;;		  :main "bookmark+.el")
    :ensure t)
(provide 'my-workspace)
