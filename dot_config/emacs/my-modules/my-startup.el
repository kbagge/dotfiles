;;; Binding maps - bind-key
;; I use bind-key to make to maps with each two prefixes. Then later I assign commands to each map.
(bind-keys
 :prefix-map my-notes-map
 :prefix "C-c n"
 :prefix "M-n")

(bind-keys
 :prefix-map my-open-map
 :prefix "C-c o"
 :prefix "M-o")

(bind-keys
 :prefix-map my-insert-map
 :prefix "C-c i")

(bind-keys :map my-insert-map
           ("c" . insert-char)
           )

;;;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;;;; Restart emacs
  (use-package restart-emacs
    :defer t)

  
(provide 'my-startup)
