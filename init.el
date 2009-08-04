; enable Common Lisp support
(require 'cl)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/customizations")
(add-to-list 'load-path "~/.emacs.d/utilities")
(add-to-list 'load-path "~/.emacs.d/vendor")

; add function to load all elisp files in a directory
(load-file "~/.emacs.d/load-directory.el")

; load all utilities
(mapcar 'load-directory
  '("~/.emacs.d/utilities"))

; load vendor libraries
(vendor 'color-theme)

; load all customizations
(mapcar 'load-directory
  '("~/.emacs.d/customizations"))

; run as server
(server-start)

(provide 'init)