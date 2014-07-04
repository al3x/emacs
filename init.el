; env
(setq explicit-shell-file-name "/bin/bash")
(setq exec-path (append '(
                          "~/bin"
                          "/usr/local/sbin"
                          "/usr/local/bin"
                          "~/.rbenv/shims/"
                          "/sbin"
                          "/opt/local/bin"
                          "/usr/local/share/npm/bin"
                          "~/.cabal/bin"
                          "/usr/local/MacGPG2/bin"
                          "~/src/mygo/bin"
                          )
                        exec-path))
(setenv "PATH"
        (mapconcat 'identity exec-path path-separator))
(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")
(setenv "ALTERNATE_EDITOR" "emacs")
(setenv "PROMPT_COMMAND" "")
(setenv "PS1" "${debian_chroot:+($debian_chroot)}\\u@\\h:\\w \\$ ")

(setenv "GIT_EDITOR" "emacsclient")
(setenv "GIT_COMMITTER_NAME" "Alex Payne")
(setenv "GIT_COMMITTER_EMAIL" "al3x@al3x.net")
(setenv "GIT_AUTHOR_NAME" "Alex Payne")
(setenv "GIT_AUTHOR_EMAIL" "al3x@al3x.net")

(setq default-directory "~/src")

; add directories to the load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/vendor")

; handy function to load all elisp files in a directory
(load-file "~/.emacs.d/utilities/load-directory.el")

; load all utility functions
(mapc 'load-directory '("~/.emacs.d/utilities"))

; load personal customizations (keybindings, colors, etc.)
(mapc 'load-directory '("~/.emacs.d/customizations"))

; per-OS customizations
(if (string-equal system-type "gnu/linux")
    (mapcar 'load-directory '("~/.emacs.d/linux-customizations"))
    (mapcar 'load-directory '("~/.emacs.d/mac-customizations")))

; load and run El-Get
(load-file "~/.emacs.d/el-get.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors (--map (solarized-color-blend it "#002b36" 0.25) (quote ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(magit-diff-use-overlays nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#bb4914") (60 . "#b0600c") (80 . "#b58900") (100 . "#947d04") (120 . "#8b8004") (140 . "#838304") (160 . "#7b8603") (180 . "#859900") (200 . "#588c2f") (220 . "#4c8d44") (240 . "#3f8f5a") (260 . "#329070") (280 . "#2aa198") (300 . "#2386a0") (320 . "#2281ad") (340 . "#217bba") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "#559944"))) t)
 '(diff-context ((t nil)) t)
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))) t)
 '(diff-function ((t (:foreground "#00bbdd"))) t)
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))) t)
 '(diff-hunk-header ((t (:foreground "#fbde2d"))) t)
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))) t)
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))) t)
 '(diff-removed ((t (:foreground "#de1923"))) t))
