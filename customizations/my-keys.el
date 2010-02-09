; magit
(global-set-key (kbd "C-c i") 'magit-status)

; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; to be able to C-x without going all the way to 'x' which sucks on
; a Dvorak keyboard layout
(keyboard-translate ?\C-t ?\C-x)

; To be able to M-x without meta - yes, this overwrites the usual
; way of exiting Emacs
(global-set-key (kbd "C-x C-c") 'execute-extended-command)
(global-set-key (kbd "C-x c") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)
