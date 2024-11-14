(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :config

  (setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
      mu4e-update-interval (* 10 60) ; check mail 10 minutes
      mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
      mu4e-get-mail-command "mbsync -a"
      mu4e-maildir "~/mail/")

  (setq mu4e-drafts-folder "/proton/Drafts"
      mu4e-sent-folder   "/proton/Sent"
      mu4e-refile-folder "/proton/Archive"
      mu4e-trash-folder  "/proton/Trash")

  (setq mu4e-maildir-shortcuts
      '(("/proton/inbox"     . ?i)
        ("/proton/Sent"      . ?s)
        ("/proton/Trash"     . ?t)
        ("/proton/Drafts"    . ?d)
        ("/proton/Archive"  . ?a)))

  (setq message-send-mail-function 'smtpmail-send-it
      auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
      smtpmail-smtp-server "127.0.0.1"
      smtpmail-smtp-service 1025
      smtpmail-stream-type  'starttls)

  (add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge/cert.pem")))

(provide 'my-mu4e)
