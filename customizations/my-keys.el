(setq default-input-method "MacOSX")

; option/alt is meta key
(setq mac-command-key-is-meta nil)

; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; to be able to C-x without going all the way to 'x' which sucks on
; a Dvorak keyboard layuot
(keyboard-translate ?\C-t ?\C-x)

; To be able to M-x without meta - yes, this overwrites exiting but
; I don't care because I quit Apple style
(global-set-key (kbd "C-x C-c") 'execute-extended-command)
(global-set-key (kbd "C-x c") 'execute-extended-command)

; switch to shell
(global-set-key (kbd "s-0") 'ansi-term)

; search with ack
(global-set-key (kbd "s-F") 'ack)

; open file
(global-set-key [(super o)] 'find-file)

; buffer switching
(global-set-key [(super {)] 'previous-buffer)
(global-set-key [(super })] 'next-buffer)

; window switching
(global-set-key (kbd "s-`") 'other-window)

; close window
(global-set-key [(super w)] (lambda ()
  (interactive)
  (kill-buffer (current-buffer)
)))

; run Ruby tests
(add-hook 'rinari-minor-mode-hook
  (lambda ()
    (define-key rinari-minor-mode-map (kbd "s-r") 'rinari-test)))