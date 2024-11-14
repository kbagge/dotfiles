;; Some functionality uses this to identify you, e.g. GPG configuration, email
  ;; clients, file templates and snippets.
  (setq user-full-name "Kristian Bagge"
        user-mail-address "mail@kristianbagge.com")
  (setq org-crypt-key "mail@kristianbagge.com")

  ;;; GPG & Authinfo
  (require 'epg)
                                          ;(setqq epg-pinentry-mode 'loopback)
  (setq epg-user-id "mail@kristianbagge.com")
  (setq epa-file-encrypt-to "mail@kristianbagge.com")
  (setq epa-file-select-keys t)
  (setq epg-gpg-home-directory "~/.config/gnupg/")

  (setq epg-debug t)
  (setq auth-source-debug t)
  (setq auth-info-debug t)
  ;;(setq epa-file-cache-passphrase-for-symmetric-encryption 't) ; This might save me from typing the pin on yubikey many times in the same session
                                          ; The above solved it. I have kept the below just in case I need to experiment with it in the future.
                                          ;(setenv "GPG_AGENT_INFO" nil) ; Make sure emacs don't use any external pinentry. Not sure if this will affect my gpg ssh setup.

  ;; Emacs have a way of saving secrets in a PGP encrypted file and retrieve them later. I have set this to ~/.authinfo.gpg
  ;; Some good sources:
  ;; - [[https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources][Mastering emacs - auth sources]]
  ;; - [[https://www.gnu.org/software/emacs/manual/html_mono/auth.html][Emacs manual - auth sources]]
  ;; - [[https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Tips-Pass.org][Emacs from scratch - auth sources]]
  (setq auth-sources '("~/.authinfo")
        auth-source-cache-expiry nil) ; default is 7200 (2h)

  ;; Passwords
  (setq password-cache-expiry nil
        password-cache t
        plstore-cache-passphrase-for-symmetric-encryption t ; This makes org-caldav cache passwords. The default is not.
  )
(provide 'my-user-info)
