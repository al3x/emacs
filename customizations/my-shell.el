; more bash-like autocomplete
(setq eshell-cmpl-cycle-completions nil)

; automatically save history
(setq eshell-save-history-on-exit t)

; ignore version control directories when autocompleting
(setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

; can't write over prompt, that would be weird
(setq comint-prompt-read-only)

; scroll to bottom on output, more like a terminal
(setq eshell-scroll-to-bottom-on-output t)
(setq eshell-scroll-show-maximum-output t)

; colorful shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; set path
(add-hook 'eshell-mode-hook
  '(lambda nil
   (let ((path))
    (setq path "/bin:/usr/local/bin:/usr/local/mysql/bin:/usr/local/google_appengine:/usr/bin:/usr/sbin")
    (setenv "PATH" path))
   (local-set-key "\C-u" 'eshell-kill-input))
 )

; start the friggin' shell
;(eshell)
;(ansi-term)
