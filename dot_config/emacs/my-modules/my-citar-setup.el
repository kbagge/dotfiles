;;; Citar
(use-package citar
  :bind (:map org-mode-map
              ("C-c b" . org-cite-insert)
              (:map my-insert-map
                    ("b" . org-cite-insert)))
  :custom
  (citar-bibliography '("~/bib/references.bib"))
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_pdf_o"
              :face 'nerd-icons-green
              :v-adjust -0.05)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-notebook"
              :face 'nerd-icons-blue
              :v-adjust -0.05)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-quote_right"
              :face 'nerd-icons-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-links-icons
              citar-indicator-notes-icons
              citar-indicator-cited-icons))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :init
  (setq org-cite-global-bibliography +bibtex-file
        org-cite-insert-processor 'citar
        org-cite-follow-processor 'citar
        org-cite-activate-processor 'citar
        org-cite-csl-styles-dir "~/Zotero/styles"
        citar-bibliography org-cite-global-bibliography
        notes-paths (if (stringp +library-notes) (list +library-notes) +library+notes)
        ))

(unless (package-installed-p 'citar-embark)
  (package-install 'citar-embark))

(use-package citar-embark
  :after citar embark
  :no-require
  :init
  ;; Open embark with enter.
  (setq citar-at-point-function 'embark-act)
  :config (citar-embark-mode))

(unless (package-installed-p 'citar-denote)
  (package-install 'citar-denote))

(use-package citar-denote
  :custom
  ;; Allow multiple notes per bibliographic entry
  (citar-open-always-create-notes nil)
  ;; Use package defaults
  (citar-denote-file-type 'org)
  (citar-denote-subdir nil)
  (citar-denote-signature nil)
  (citar-denote-template nil)
  (citar-denote-keyword "bib")
  (citar-denote-use-bib-keywords nil)
  (citar-denote-title-format "title")
  (citar-denote-title-format-authors 1)
  (citar-denote-title-format-andstr "and")
  :init
  (citar-denote-mode)
  ;; Bind all available commands
  :bind (("C-c w c" . citar-create-note)
         ("C-c w n" . citar-denote-open-note)
         ("C-c w d" . citar-denote-dwim)
         ("C-c w e" . citar-denote-open-reference-entry)
         ("C-c w a" . citar-denote-add-citekey)
         ("C-c w k" . citar-denote-remove-citekey)
         ("C-c w r" . citar-denote-find-reference)
         ("C-c w l" . citar-denote-link-reference)
         ("C-c w f" . citar-denote-find-citation)
         ("C-c w x" . citar-denote-nocite)
         ("C-c w y" . citar-denote-cite-nocite)))

(provide 'my-citar-setup)
