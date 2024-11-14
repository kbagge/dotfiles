;;;; Show battery and time
(display-time-mode 1)              ; Enable time in the mode-line
(setq display-time-24hr-format t)
(display-battery-mode 0)           ; it's nice to know how much power you have

;;;; Modeline
 (use-package doom-modeline
   :init
   ;; Whether to use hud instead of default bar. It's only respected in GUI.
   (setq doom-modeline-hud t)
   (setq doom-modeline-buffer-encoding nil) ; No UTF-8 info in bar
   (setq doom-modeline-buffer-modification-icon nil)
   (setq doom-modeline-display-misc-in-all-mode-lines nil)
   (setq doom-modeline-percent-position nil)
   (setq line-number-mode nil)
   (setq column-number-mode nil)
   (setq display-time-default-load-average nil) ;; Remove load average from bar

   :custom
   (doom-modeline-mode 1)

     )

 (provide 'my-doom-modeline)
