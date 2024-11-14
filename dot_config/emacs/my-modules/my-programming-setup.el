;;; Programming
  ;;;; Highlight indents
  ;; Maybe this would be better under editor, I'll think about it.
  (use-package highlight-indent-guides
    :hook (prog-mode-hook . highlight-indent-guides-mode))

  ;;;; Direnv mode
  ;; Enables direnv to set $Path and env correct for dev venv's
  (use-package direnv
    :config
    (direnv-mode))


  ;;;; Eglot
  (use-package eglot
    :hook
    (python-ts-mode-hook . eglot-ensure))

                                          ; (straight-use-package 'consult-glot)
                                          ; (require 'consult-eglot)

  (use-package treesit-auto :ensure t)
  ;; treesit grammar should be installed here: "~/.emacs.d/tree-sitter/"


;;;; Vterm
;; I have switched to eat from vterm. It is a bee wit slower, but does not flicker.
(use-package eat
  :bind (:map my-open-map
              ("<return>" . 'eat)))
;; https://www.reddit.com/r/emacs/comments/1dab1b0/how_to_run_a_command_in_an_existing_eat_terminal/   <- Might use this to use pixi in eat terminal for sending python code.

(provide 'my-programming-setup)
