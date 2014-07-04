; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; C-x without going all the way to 'x',
; which sucks on Dvorak
(keyboard-translate ?\C-t ?\C-x)

; Always indent
(electric-indent-mode 1)

; TextMate-style commenting
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)
