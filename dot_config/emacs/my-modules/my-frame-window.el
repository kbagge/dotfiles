(use-package yequake
  :custom
  (yequake-frames
   '(("org-capture" 
      (buffer-fns . (yequake-org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))
     ("bookmark menu" 
      (buffer-fns . (call-interactively 'bookmark-bmenu-list))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t))))
     )))

(defmacro present (&rest body)
  "Create a buffer with BUFFER-NAME and eval BODY in a basic frame."
  (declare (indent 1) (debug t))
  `(let* ((buffer (get-buffer-create (generate-new-buffer-name "*present*")))
          (frame (make-frame '((auto-raise . t)
                 ;;              (font . "Menlo 15")
                               (top . 200)
                               (height . 20)
                               (width . 110)
                               (internal-border-width . 20)
                               (left . 0.33)
                               (left-fringe . 0)
                               (line-spacing . 3)
                               (menu-bar-lines . 0)
                               (minibuffer . only)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (undecorated . t)
                               (unsplittable . t)
                               (vertical-scroll-bars . nil)))))
 ;;    (set-face-attribute 'ivy-current-match frame
   ;;                      :background "#2a2a2a"
     ;;                    :foreground 'unspecified)
     (select-frame frame)
     (select-frame-set-input-focus frame)
     (with-current-buffer buffer
       (condition-case nil
           (unwind-protect
               ,@body
             (delete-frame frame)
             (kill-buffer buffer))
         (quit (delete-frame frame)
               (kill-buffer buffer))))))


(defun present-open-bookmark-frame ()
  (present (call-interactively 'bmkp-url-jump)))
(defun present-save-bookmark-frame ()
  (present (call-interactively 'bmkp-url-target-set)))
(defun present-bookmark-menu-frame ()
  (present (call-interactively 'bookmark-bmenu-list)))

  (provide 'my-frame-window)
