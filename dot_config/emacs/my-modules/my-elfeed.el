;;; Office
    ;;;; Elfeed

(use-package elfeed
  :defer t
  :custom
  (setq-default elfeed-search-filter "@1-week-ago +unread ")
  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :before "2 weeks ago"
                                :remove 'unread))
  ;; Dual pane
  (setq elfeed-show-entry-switch #'elfeed-display-buffer)

  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

  ;; Easy tagging
  (defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
          MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

  ;; Tag with a single letter keybind:

  (define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
  (define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))

  ;; Open links without browser
  (defun elfeed-show-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)))

  (defun elfeed-search-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p)))

  (define-key elfeed-show-mode-map (kbd "B") 'efleed-show-eww-open)
  (define-key elfeed-search-mode-map (kbd "B") 'efleed-search-eww-open)

  ;; Youtube in mpv
  (setq browse-url-browser-function
        '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
          ("." . browse-url-generic)))

  (defun browse-url-mpv (url &optional single)
    (start-process "mpv" nil "mpv" (shell-quote-argument url)))
  )

;;  (use-package elfeed-web)

(use-package elfeed-org
  :commands elfeed
  :custom
  (rmh-elfeed-org-files (list "~/org/20240101T172750--elfeed__elfeed.org")))

(provide 'my-elfeed)
