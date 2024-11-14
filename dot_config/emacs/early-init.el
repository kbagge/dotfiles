;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection
  (setq gc-cons-threshold most-positive-fixnum)    ; Very large threshold for garbage
                                               ; collector during init
  ;; Reset garbage collector limit after init process has ended (8Mo)
  (add-hook 'after-init-hook
            #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)
(setq package-install-upgrade-built-in t)
(require 'use-package)

;; vc-use package. Is part of emacs 30.something. I'm using 29, so I install manually.
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Menu bar disabled
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
