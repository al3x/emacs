; font
(set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-")

; don't display startup message
(setq inhibit-startup-message t)

; no toolbar or scrollbar
(scroll-bar-mode -1)
(tool-bar-mode -1)

; blink cursor
(blink-cursor-mode)

; force new frames into existing window
(setq ns-pop-up-frames nil)

; no bell
(setq ring-bell-function 'ignore)

; terminal color
(require 'ansi-color)

; theme
(require 'color-theme)
(setq color-theme-is-global t)
(load-file "~/.emacs.d/vendor/twilight-emacs/color-theme-twilight.el")
(color-theme-twilight)

(provide 'my-ui)