; can't write over prompt, that would be weird
(setq comint-prompt-read-only)

; colorful shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
