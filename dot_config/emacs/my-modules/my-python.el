;;;; Python
;; Some nice resources:
;; - [[https://www.adventuresinwhy.com/post/eglot/][Eglot + treesitter in emacs (python)]]
;; - [[https://gist.github.com/habamax/290cda0e0cdc6118eb9a06121b9bc0d7][Same but other source]]
;; - [[https://www.masteringemacs.org/article/how-to-get-started-tree-sitter][Mastering emacs on treesitter]]

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)))

;; Remember to install pyright
 (use-package lsp-pyright ; Server for eglot.
   :hook (python-ts-mode . (lambda ()
                          (require 'lsp-pyright))))

;;(add-hook 'python-mode-hook 'eglot-ensure)
;;  (add-to-list 'eglot-server-programs
;;               '(python-ts-mode . ("ruff" "server")))
;;  (add-hook 'after-save-hook 'eglot-format))

;; https://docs.astral.sh/ruff/editors/setup/#emacs
(use-package flymake-ruff
  :ensure t
  :hook (python-ts-mode . flymake-ruff-load))

(use-package ruff-format
  :ensure t
  :hook (python-ts-mode . ruff-format-on-save-mode))


;; See comments here on reddit:
;; https://www.reddit.com/r/emacs/comments/17g1jw3/what_is_yours_configuration_for_python/
;; (use-package conda
;;   :defer t
;;   :init
;;   (setq conda-anaconda-home "~/.mambaforge/")
;; ;; ----- select interpreter

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-arg " -i --simple-prompt --InteractiveShell.display_page=True"
      python-shell-completion-native-enable nil)

;; )

;; ----- emacs-jupyter
;; Disabled since it did not work right
;;  (use-package jupyter
;; ;;   :init
;; ;;   (setq jupyter-use-zmq nil)
;;    ;; :commands
;;    ;; (jupyter-run-server-repl
;;    ;;  jupyter-run-repl
;;    ;;  jupyter-server-list-kernels)
;;    :custom
;;    (jupyter-executable "/.pixi run jupyter kernel")
;;    )
;;  ;;(org-babel-jupyter-override-src-block "python") ;; so python becomes jupyter-python in org-babel

(provide 'my-python)
