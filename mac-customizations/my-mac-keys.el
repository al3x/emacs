(setq default-input-method "MacOSX")

; don't use Lion press-and-hold
(ns-set-resource nil "ApplePressAndHoldEnabled" "NO")

; option/alt is meta key
(setq mac-command-key-is-meta nil)

; switch to shell
(global-set-key (kbd "s-0") 'visit-ansi-term)

; open file
(global-set-key [(super o)] 'find-file)

; buffer switching
(global-set-key [(super {)] 'previous-buffer)
(global-set-key [(super })] 'next-buffer)

; frame switching
(global-set-key (kbd "s-`") 'other-window)

; indenting and outdenting
(defun textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
  A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))

(defun textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (textmate-shift-right (* -1 (or arg 1))))

(global-set-key (kbd "s-[") 'textmate-shift-left)
(global-set-key (kbd "s-]") 'textmate-shift-right)

; close window
(global-set-key [(super w)] (lambda ()
  (interactive)
  (kill-buffer (current-buffer)
)))

; navigating through errors
(global-set-key [(meta n)] 'next-error)
(global-set-key [(meta p)] 'previous-error)
