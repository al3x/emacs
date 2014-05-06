;;; shell-here-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (shell-here) "shell-here" "shell-here.el" (21073
;;;;;;  62812 0 0))
;;; Generated autoloads from shell-here.el

(autoload 'shell-here "shell-here" "\
Open a shell relative to `default-directory'.

With no argument, open a shell in `default-directory'.
With a positive numeric argument, open a shell ARG levels up from
`default-directory'.
With a plain negative argument, open a shell in the project root.
With a negative numeric argument, open a shell ARG levels up from the
project root.

Shell buffer names include the name of the current project's
directory, if available; otherwise *shell*. If a shell buffer already
exists, it will be reused.

With the universal argument, open a new shell in `default-directory'.
With a negative universal argument, open a new shell in the project
root.

Project root is determined with `ffip-project-root', if available.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("shell-here-pkg.el") (21073 62812 786256
;;;;;;  0))

;;;***

(provide 'shell-here-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; shell-here-autoloads.el ends here
