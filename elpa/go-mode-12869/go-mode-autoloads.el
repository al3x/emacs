;;; go-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (godoc gofmt-before-save gofmt go-mode) "go-mode"
;;;;;;  "go-mode.el" (20405 20016))
;;; Generated autoloads from go-mode.el

(autoload 'go-mode "go-mode" "\
Major mode for editing Go source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides indentation that is
\(almost) identical to gofmt.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go$" #'go-mode))

(autoload 'gofmt "go-mode" "\
Pipe the current buffer through the external tool `gofmt`.
Replace the current buffer on success; display errors on failure.

\(fn)" t nil)

(autoload 'gofmt-before-save "go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook #'gofmt-before-save)

\(fn)" t nil)

(autoload 'godoc "go-mode" "\
Show go documentation for a query, much like M-x man.

\(fn QUERY)" t nil)

;;;***

;;;### (autoloads nil nil ("go-mode-load.el" "go-mode-pkg.el") (20405
;;;;;;  20016 146199))

;;;***

(provide 'go-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; go-mode-autoloads.el ends here
