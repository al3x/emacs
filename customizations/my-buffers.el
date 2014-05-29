; use ibuffer instead of the built in buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

; dynamic expansion tweaks
(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
         (remove 'try-expand-line hippie-expand-try-functions-list)))

; autosave files in tmp dir
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

; disable backup files (foo~)
(setq backup-inhibited t)

; save cursor position within files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")
(setq-default save-place t)

; save minibuffer history across sessions
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)

; nicer naming of buffers with identical names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; Interactively Do Things
(require 'ido)
(ido-mode t)
(icomplete-mode t)
(ido-init-completion-maps)
(setq ido-enable-flex-matching t) ; case insensitive matching
(add-to-list 'ido-ignore-files "\\.DS_Store")
(setq ido-create-new-buffer 'always) ; always create a new buffer with Ido
(setq ido-use-virtual-buffers t)
(setq confirm-nonexistent-file-or-buffer nil)

; automatically clean up old buffers
(require 'midnight)

; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)

; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

; yes, I want to kill buffers with processes attached
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
