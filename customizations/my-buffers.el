; recent files
(require 'recentf)
(setq recentf-max-saved-items 100)

; dired
;(require 'dired+)
;(setq dired-recursive-deletes 'top)
;(define-key dired-mode-map [mouse-2] 'dired-find-file)

; use ibuffer instead of the built in buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

; dynamic expansion tweaks
(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
         (remove 'try-expand-line hippie-expand-try-functions-list)))

; desktop saving
(setq desktop-path '("~/.emacs.d"))
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

; disable auto-save files (#foo#)
(setq auto-save-default nil)

; disable backup files (foo~)
(setq backup-inhibited t)

; save place
(require 'saveplace)

; nicer naming of buffers with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
