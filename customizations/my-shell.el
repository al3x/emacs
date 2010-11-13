; sane path
(setq path "/bin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/mysql/bin:/usr/local/google_appengine")
(setenv "PATH" path)

; can't write over prompt, that would be weird
(setq comint-prompt-read-only)

; colorful shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq ansi-term-color-vector [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65" "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])