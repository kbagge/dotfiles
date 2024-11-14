;;;; Spelling
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("<f7>" . jinx-correct)
         ("S-<f7>" . jinx-languages))
  :init
  (setq jinx-languages '"en_US")
  ;; (add-to-list 'vertico-multiform-categories
  ;;          '(jinx grid (vertico-grid-annotate . 20)))
  ;; (vertico-multiform-mode 1)
  )

;;;; Grammar

;;   Maybe also check out writegood mode
;; There are two packages that doom emacs use for grammar checking. One of them can also be used online to check for example a manuscript.
;; Languagetool and writegood mode.
;; Remember to install languagetool.

(use-package writegood-mode
  :bind ("C-C g" . writegood-mode))

(use-package langtool
  :defer t)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")
(bind-keys
 :prefix-map languagetool-map
 :prefix "C-c l")
(bind-keys :map languagetool-map
           ("c" . langtool-interarctive-correction)
           ("d" . langtool-check-done)
           ("h" . langtool-show-message-at-point)
           ("l" . langtool-check)
           )
(provide 'my-spelling)
