; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; C-x without going all the way to 'x',
; which sucks on Dvorak
(keyboard-translate ?\C-t ?\C-x)

; Always indent
(electric-indent-mode 1)

; TextMate-style commenting
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

; Easily move lines up and down using utility functions
(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

; Compile
(global-set-key (kbd "C-c c") 'compile)
