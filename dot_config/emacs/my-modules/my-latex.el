;;;; Latex
;; Some options to highlight faces for latex export:
(use-package engrave-faces
  :init
  (setq org-latex-src-block-backend 'engraved)
  (setq org-latex-engraved-theme 'doom-one-light)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6)))

(provide 'my-latex)
