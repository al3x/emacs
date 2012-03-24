; enable Common Lisp support
(require 'cl)

; Marmalade package manager
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

; some modes need to call stuff on the exec-path
(push "/usr/local/bin" exec-path)

; add directories to the load path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/vendor")

; handy function to load all elisp files in a directory
(load-file "~/.emacs.d/load-directory.el")

; load utility functions
(mapcar 'load-directory '("~/.emacs.d/utilities"))

; load third-party modes
; note: these are configured in customizations/my-modes.el
(vendor 'centered-cursor-mode)
(vendor 'yaml-mode)
(vendor 'textile-minor-mode)
(vendor 'save-visited-files)
(vendor 'dired+)
(vendor 'thrift-mode)

; load third-party modes that the vendor function can't handle
(add-to-list 'load-path "~/.emacs.d/vendor/scala")

; load personal customizations (keybindings, colors, etc.)
(mapcar 'load-directory '("~/.emacs.d/customizations"))

; per-OS customizations
(if (string-equal system-type "gnu/linux")
    (mapcar 'load-directory '("~/.emacs.d/linux-customizations"))
    (mapcar 'load-directory '("~/.emacs.d/mac-customizations")))

; start a server for usage with emacsclient
;(add-hook 'after-init-hook 'server-start)
