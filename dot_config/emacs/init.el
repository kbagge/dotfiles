;; Add the folder with modules to load path
 (mapc
  (lambda (string)
    (add-to-list 'load-path (locate-user-emacs-file string)))
  '("my-modules")) ; More folders can be added if nedded '("example1" "example2"))

 ;; General
 (require 'my-startup)
 (require 'my-defaults)
 (require 'my-dired)
 ;; Theming
 (require 'my-modus-theme)
 (require 'my-general-theming)
 (require 'my-fonts)
;; (require 'my-modeline)
(require 'my-doom-modeline)
 ;; Editor
 (require 'my-editor-settings)
 (require 'my-workspace)
 (require 'my-spelling)
 (require 'my-completion)
 (require 'my-minibuffer)
 ;; User
 (require 'my-user-info)
 (require 'my-org-setup)
 ;; Notes
 (require 'my-denote-setup)
 (require 'my-citar-setup)
 ;; Office
 (require 'my-office-setup)
 (require 'my-elfeed)
 (require 'my-pocket)
 (require 'my-mu4e)
 ;; Programming
 (require 'my-programming-setup)
 (require 'my-python)
 (require 'my-chezmoi)
 (require 'my-latex)
 (require 'my-git)
 (require 'my-llm)
