; enable Common Lisp support
(require 'cl)

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
(vendor 'color-theme)
(vendor 'textmate)
(vendor 'nav)
(vendor 'centered-cursor-mode)
(vendor 'browse-kill-ring)
(vendor 'yaml-mode)
(vendor 'rinari)
(vendor 'ack)
(vendor 'textile-minor-mode)
(vendor 'magit)
(vendor 'save-visited-files)
(vendor 'dired+)
(vendor 'minimap)
(vendor 'clojure-mode)
(vendor 'slime)

; load third-party modes that the vendor function can't handle
(add-to-list 'load-path "~/.emacs.d/vendor/scala")
(add-to-list 'load-path "~/.emacs.d/vendor/js2")
(add-to-list 'load-path "~/.emacs.d/vendor/swank-clojure/src/emacs")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/contrib/lisp")

; load personal customizations (keybindings, colors, etc.)
(mapcar 'load-directory '("~/.emacs.d/customizations"))

; per-host customizations
(let ((hostname (shell-command-to-string "/bin/hostname")))
  (if (eq hostname "jaya.local")
      (message "on work machine")
      (load-file "~/.emacs.d/office_tweaks.el")
    ))

; run server for emacsclient interactions
;(server-start)
