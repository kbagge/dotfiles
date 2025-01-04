(use-package org-re-reveal
:config
;; Use the web version of reveal, no installation hazzle and stays up to date.
(setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-re-reveal-revealjs-version "4")
;; Settings

)


(use-package quarto-mode
  :config
  (setq quarto-command "~/opt/quarto-1.6.39/bin/quarto"))
