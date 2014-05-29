; can't write over prompt, that would be weird
(setq comint-prompt-read-only)

; colorful shell
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq ansi-term-color-vector [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65" "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])
