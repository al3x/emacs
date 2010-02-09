; switch to shell
(global-set-key (kbd "M-0") 'ansi-term)

; search with ack
(global-set-key (kbd "M-F") 'ack)

; open file
(global-set-key (kbd "M-o") 'find-file)

; buffer switching
(global-set-key (kbd "M-{") 'previous-buffer)
(global-set-key (kbd "M-}") 'next-buffer)

; window switching
(global-set-key (kbd "M-`") 'other-window)

; close window
(global-set-key (kbd "M-w") (lambda ()
  (interactive)
  (kill-buffer (current-buffer)
)))

; save buffer
(global-set-key (kbd "M-s") 'save-buffer)

; navigating through errors
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

; paste
(global-set-key (kbd "M-v") 'yank)

; undo
(global-set-key (kbd "M-z") 'undo)

; run Ruby tests, TextMate-style
(add-hook 'rinari-minor-mode-hook
  (lambda ()
    (define-key rinari-minor-mode-map (kbd "M-r") 'rinari-test)))
