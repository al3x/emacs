(setq default-input-method "MacOSX")

; option/alt is meta key
(setq mac-command-key-is-meta nil)

; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)

; switch to shell
(global-set-key (kbd "s-0") 'eshell)

; search with ack
(global-set-key (kbd "s-F") 'ack)

; open file
(global-set-key [(super o)] 'find-file)

; buffer switching
(global-set-key [(super {)] 'previous-buffer)
(global-set-key [(super })] 'next-buffer)

; close window
(global-set-key [(super w)] (lambda ()
  (interactive)
  (kill-buffer (current-buffer)
)))

; run Ruby tests
(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (define-key rinari-minor-mode-map (kbd "s-r") 'rinari-test)))