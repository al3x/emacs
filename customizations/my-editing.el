; tabs and indentation
(setq standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

; mousing
(setq mac-emulate-three-button-mouse nil)
(pc-selection-mode t)

; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

; whitespace
;(global-whitespace-mode t)
;(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; line numbering
;(require 'linum)
;(global-linum-mode 0)
;(setq linum-format "%d ") ; space after line number

; show column number in bar
(column-number-mode t)

; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

; selection
(delete-selection-mode t)

; show marks as selections
(setq transient-mark-mode t)

; highlight matching parens
(show-paren-mode t)

; highlight incremental search
(defconst search-highlight t)

; no newlines past EOF
(setq next-line-add-newlines nil)

; apply syntax highlighting to all buffers
(global-font-lock-mode t)

; smooth scrolling
;(require 'centered-cursor-mode)
;(global-centered-cursor-mode t)

; wrap lines in a tasteful way
(global-visual-line-mode 1)

; column width
;(set-fill-column 100)
;(setq default-fill-column 100)