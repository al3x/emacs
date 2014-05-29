; Make yes-or-no questions answerable with 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)

; to be able to C-x without going all the way to 'x' which sucks on
; a Dvorak keyboard layout
(keyboard-translate ?\C-t ?\C-x)
