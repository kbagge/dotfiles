;;; Org mode
  ;;;; Org variables
  ;; Define new variables to call later
  (setq +org-dir (concat (getenv "HOME") "/org/")
        +agenda-dir (concat +org-dir "agenda/")
        +org-image-dir "~/Library/Images/"
        +library-notes (concat +org-dir "library-notes/")
        +bibtex-file '("~/org/library-notes/My_Library.bib")
        +org-projects-todo-file (concat +agenda-dir "20231227T222442--projects__agenda_projects.org")
        +org-capture-recipies-file  (concat +org-dir "20240102T010106--kogebog__mad.org")
        +org-capture-inbox (concat +agenda-dir "20231226T221108--inbox__agenda_inbox.org"))
  ;; Set org-dir and org-archive location
  (setq org-directory +org-dir
        org-archive-location (concat +org-dir "archive.org_archive::* From %s"))

  (use-package org
    :init
    ;; UI interface
    (setq org-hide-emphasis-markers t)
    (setq org-pretty-entities t)
    (setq org-startup-indented t)
    (setq org-pretty-entities-include-sub-superscripts nil)
    (setq org-startup-folded "show1levels")
    ;; Use RET to open org-mode links, including those in quick-help.org
    (setq org-return-follows-link t)

    :custom
    (require 'org-crypt)
    (setq org-tags-exclude-from-inheritance
          '("crypt"))
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch) ;; Make tables use fixed pitch
    :hook (org-mode . visual-line-mode)
    (org-mode . variable-pitch-mode)
    :bind
    ("C-c c" . org-capture)
    ("C-c a" . org-agenda)
    (:map my-open-map
          ("a" . org-agenda))
    )

  (use-package org-modern
    :after org
    :config
    (global-org-modern-mode)
    :init
    ;; (setq org-modern-timestamp '(" %A, %b %e - %Y " . " %H:%M "))
    (setq org-modern-timestamp nil)
    (setq org-modern-block-fringe 1)
    (setq org-modern-table nil)
    (setq org-modern-star '("✳" "○" "◉" "◇" "◈"))
    )

  (setq org-todo-keywords
        '((sequence
           "TODO"  ; A task that needs doing & is ready to do
           "STARTED(!)" ; Something I started on ! Means the time started is logged in txt.
           "|"
           "DONE"  ; Task successfully completed
           "CANCELED" ; Explains itself - used before archiving.
           )))

  (setq org-todo-keyword-faces
        '(("TODO" . org-warning) ("STARTED" . "yellow")
          ("CANCELED" . (:foreground "blue" :weight bold))))

  (setq org-tag-persistent-alist '(("work" . ?w) ("home" . ?h) ("@online" . ?o) ("@møde" . ?m) ("@errand" . ?e) ("@phone" . ?p) (:newline)
                                   ("blocked" .?b) ("someday" .?s) (:newline)
                                   ("anders" . ?A) ("simone" .?S) ("maria" . ?M) ))
  (setq org-special-ctrl-a/e t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp - t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)

  (use-package org-pomodoro)
  (setq org-pomodoro-manual-break t)

  (use-package org-download
    :after org
    :init
    (setq org-attach-id-dir (file-name-as-directory(concat +org-dir "data"))
          org-download-image-dir +org-image-dir
          org-image-actual-width (/ (display-pixel-width) 3) ; vil gøre alle billeders bredde svarende til 1/3 af skærmen.
                                          ;org-image-actual-width '(300) ; vil gøre alle billeder 300 pixel bredde
          org-startup-with-inline-images t ;; gør nok org lidt langsommere, men jeg bruger det i mange af mine noter.
          org-link-abbrev-alist '(("image-file" . "~/Library/Images/%s")
                                  ("wikipedia" . "https://en.wikipedia.org/wiki/%s")
                                  ("duckduckgo" . "https://duckduckgo.com/?q=%s")
                                  ("gmap" . "https://maps.google.com/maps?q=%s")
                                  ("gimages" . "https://google.com/images?q=%s")
                                  ("google" . "https://google.com/search?q=")
                                  ("youtube" . "https://youtube.com/watch?v=%s")
                                  ("github" . "https://github.com/%s"))
          ))

;;;; Math latex in org-mode inline
(use-package org-fragtog
  :custom
  ;; Math looks small in org-mode - lets make latex fragments bagger
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  :hook (org-mode . org-fragtog-mode)
  )

(use-package org-super-agenda
  :hook (org-agenda-mode . org-super-agenda-mode))

(setq org-agenda-files (directory-files-recursively +agenda-dir ".*\.org")
      org-agenda-dim-blocked-tasks nil  ; The following are supposed to speed up org-agenda when it gets slow - see https://orgmode.org/worg/agenda-optimization.html -- check the documentation, I don't need it.
      ;; org-agenda-inhibit-startup t   ; agenda doesnt respect folding, making it faster. <- disabled for now since it might not be neccessary.
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-include-deadlines t
      org-deadline-warning-days 7 ; for the calendar view
      org-agenda-todo-ignore-deadline 14 ; for the todo list
      org-agenda-todo-ignore-scheduled 7 ; use less warning days for scheduled tasks
      org-agenda-start-day "+0d"
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t
      org-log-done 'time ;; puts a time log on when an item was marked as done
      ;;      org-agenda-hide-tags-regexp (regexp-opt '("no_roam"))

      org-agenda-prefix-format ;; Remove the file coloumn in the agenda and todo view
      '((agenda . " %i %?-12t% s")
        (todo   . " %i %s")
        (tags   . " %i ")
        (search . " %i %-12:c"))
      )

;; The custom agenda command follows below - maybe one day make more for different purposes
(setq  org-agenda-custom-commands
       '(("o" "Overview"
          ((agenda "" ((org-agenda-format-date (format-time-string "%A     %e %B %Y  --  (uge %W)" (current-time)))
                       (org-agenda-span 1)
                       (org-agenda-include-deadlines nil)
                       (org-habit-show-habits-only-for-today nil)
                       (org-super-agenda-groups
                        '((:name "Today"
                                 :time-grid t
                                 :date today
                                 :todo "TODO"
                                 :scheduled past
                                 :order 1)))))
           (agenda "" ((org-agenda-span 1)
                       (org-agenda-format-date "")
                       (org-agenda-start-day "+1d")
                       (org-habit-show-habits-only-for-today nil)
                       (org-agenda-include-deadlines nil)
                       (org-agenda-use-time-grid nil)
                       (org-super-agenda-groups
                        '((:name "Tomorrow:"
                                 :date today
                                 :deadline nil
                                 :scheduled past
                                 :order 1)))))
           (agenda "" ((org-agenda-format-date "")
                       (org-agenda-span 1)
                       (org-agenda-use-time-grid nil)
                       (org-agenda-todo-ignore-deadlines 'far)
                       (org-super-agenda-groups
                        '((:name "Deadlines:"
                                 :deadline t
                                 :discard (:anything t)
                                 :order 1)))))
           (alltodo "" ((org-agenda-overriding-header "")
                        (org-agenda-tag-filter-preset '("-someday"))
                        (org-super-agenda-groups
                         '((:name "In Process"
                                  :todo "STARTED"
                                  :discard (:habit t)
                                  :order 1)
                           (:name "Due Today"
                                  :deadline today
                                  :discard (:todo "[ ]")
                                  :order 2)
                           (:name "Top priority"
                                  ;;                             :tag "Important"
                                  :priority "A"
                                  :order 6)
                           (:name "Quick tasks"
                                  :and(:effort< "0:05" :not (:tag "blocked"))
                                  :order 7)
                           (:name "Work"
                                  :and (:tag "work" :todo "TODO" :not (:tag "mail") :not(:tag "blocked") :not(:tag "ykm"))
                                  :order 10)
                           (:name "YKM"
                                  :tag "ykm"
                                  :order 11)
                           (:name "Personal"
                                  :and (:tag "me" :not (:tag "blocked"))
                                  :order 12)
                           (:name "Mail"
                                  :tag "mail"
                                  :order 13)
                           (:name "Projects"
                                  :and (:todo "TODO" :tag "project" :not (:tag "emacs") :not(:tag "blocked"))
                                  :order 14)
                           (:name "Research"
                                  :and (:tag "research" :todo "TODO" :not(:tag "blocked"))
                                  :order 15)
                           (:name "To read"
                                  :tag "read"
                                        ;                               :todo "TODO"
                                  :order 30)
                           (:name "emacs"
                                  :tag "emacs"
                                  :order 40)
                           (:name "Linux"
                                  :and (:tag "arch"
                                             :todo "TODO")
                                  :order 50)
                           (:name "Other items"
                                  :not (:tag "blocked")
                                  :order 55)
                           (:name "Blocked Tasks"
                                  :tag "blocked"
                                  :order 60)
                           ))))
           ))))

;;; Org capture
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
             Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (propertize "" 'font-lock-face '(:foreground "red"))  "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

             TABLE is the alist which should contain entries where the car is a string.
             There should be two types of entries.

             1. prefix descriptions like (\"a\" \"Description\")
                This indicates that `a' is a prefix key for multi-letter selection, and
                that there are entries following with keys like \"ab\", \"ax\"…

             2. Select-able members must have more than two elements, with the first
                being the string of keys that lead to selecting it, and the second a
                short description string of the item.

             The command will then make a temporary buffer listing all entries
             that can be selected with a single key, and all the single key
             prefixes.  When you press the key for a single-letter entry, it is selected.
             When you press a prefix key, the commands (and maybe further prefixes)
             under this key will be shown and offered for selection.

             TITLE will be placed over the selection in the temporary buffer,
             PROMPT will be used when prompting for a key.  SPECIALS is an
             alist with (\"key\" \"description\") entries.  When one of these
             is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                                          ;                                (insert prefix "[" k "]" "..." "  " desc "..." "\n"))) ;; original
                           (insert prefix " " k " " "..." "  " desc "..." "\n"))) ;; My version
                                          ;                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize " ›" 'face 'font-lock-comment-face) "  " desc "…" "\n"))) ;; teco version
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                                          ;                                          (insert prefix "[" k "]" "     " desc "\n") ;; original
                           (insert prefix " " k " " "     " desc "\n") ;; My version
                                          ;                            (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys))) ;; teco version
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                                          ;              (insert (format "%s   %s\n" (propertize key 'face '(bold nerd-icons-red)) description)) ;; teco version
                                          ;                                       (insert (format "[%s]     %s\n" key description)) ;; original
                      (insert (format " %s      %s\n" key description)) ;; My version
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)

  (use-package org-project
    :vc (:fetcher github
                  :repo "delehef/org-project")

    :after org)


  ;; Change the name of the heading in my project.org file, and add a tag with the name of the project.
  (defun org-project--build-heading (projectpath)
    "Create an org heading for PROJECTPATH."
    (let* ((raw-heading (org-project--name-from-project projectpath))
           (heading-linkized (if org-project-link-heading
                                 ;;     (org-project--linkize-heading raw-heading projectpath) ;; original
                                 (org-project--linkize-heading projectpath projectpath) ;; my version - that gives full path instead of only name of dir.
                               raw-heading))
           (heading-final (concat heading-linkized " :" raw-heading ":"))) ;; My version - that adds a tag with the project dir name.
      ;;        (heading-final heading-linkized)) ;; original
      heading-final))


  ;; If invoked outside of a project, prompt for a valid project to capture for
  (setq org-project-prompt-for-project t) ;; Maybe this tries to call org-project when opening?

  ;; Store all TODOs in a ORG_DIRECTORY/project.org
  (setq org-project-todos-per-project nil)
  (setq org-project-todos-file +org-projects-todo-file)

  ;; Use custom capture templates
  (setq org-project-capture-template "* TODO %? \n%t\n") ;; Ask for a TODO and a date
  ;; (setq org-project-quick-capture-template "* TODO %? %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n") ;; Quick TODOs ae scheduled in two days

  ;; Add some binding for org-project in project.el map
  (bind-keys :map project-prefix-map
             ("t" . org-project-quick-capture)
             ("T" . org-project-capture)
             ("o" . org-project-open-todos))

  (setq org-capture-templates `(("a" " interesting Article" entry (file ,(concat +org-dir "daily/" (format-time-string "%Y-%m-%d.org")))
                                 "* %? :read:article:\n[[id:b57b817b-a5a5-4c98-9f95-068de01a66ea][Reading list]]")
                                ("t" " Personal todo" entry (file +org-capture-inbox)
                                 "* TODO %?\n%i %a")
                                ("n" " Quick note" entry (file+headline ,(concat +org-dir "daily/" (format-time-string "%Y-%m-%d.org")) "Notes")
                                 "* %?\n%i %a")
                                ("e" " Email" entry (file+headline +org-capture-inbox "Tasks")
                                 "* TODO Send email til %\\1 %? :mail:\nSend email til %^{modtager} vedr. %^{vedrørende}\n%U %i %a  ")
                                ("l" " Link" entry (file+headline ,(concat +org-dir "Bookmarks.org") "Unsorted")
                                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \nCaptured On: %U")
                                ;; ("i" " Interesting") 
                                ;; ("ii" " Idea" entry (file+headline ,(concat +org-dir "daily/" (format-time-string "%Y-%m-%d.org")) "Notes")
                                ;;  "* %? :idea:\n#[[id:0f3c3087-aab5-408a-b47f-20f3fb7c0f7e][Inbox]]")
                                ;; ("p" " Projects")
                                ;; ("pt" " Project todo" entry (file+headline +org-projects-todo-file ,(org-project--build-heading (org-project--current-project))))
                                ;;                     "* TODO %?\n%i %a")
                                ;;    ("pn" " Project note" entry (file+headline ,(concat (org-project--current-project) "notes.org") "Notes")
                                ;;    "* %?\n%i %a" :unnarrowed t)
                                ))

  (setq org-refile-targets '(("/home/bagge/org/agenda/20231226T221108--inbox__agenda_inbox.org"  :maxlevel . 3)
                             ("/home/bagge/org/agenda/20231226T220621--organizer__agenda_metanote_organizer.org" :maxlevel . 3)
                             ("/home/bagge/org/agenda/20231227T222442--projects__agenda_projects.org" :maxlevel . 3)
                             (nil :maxlevel . 3)))
  ;; I also want to be able to set refiles in at a topline level
  (setq org-refile-use-outline-path 'file) ; Allow refiling to files
  (setq org-outline-path-complete-in-steps nil) ; But not only to files, we still want headings.
  (setq org-refile-allow-creating-parent-nodes 'confirm) ; Allow to refile to non-existent headings.

(provide 'my-org-setup)
