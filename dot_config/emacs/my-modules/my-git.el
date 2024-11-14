;;;; Magit
;; Maybe check out forge at some point to interact with github.
;; and git-timemachine: https://codeberg.org/pidu/git-timemachine
(use-package magit

  :init
  (setq vc-follow-symlinks t) ;; Since I track my init file with git, I don't need a warning every time I open emacs.
  :defer t)

(provide 'my-git)
