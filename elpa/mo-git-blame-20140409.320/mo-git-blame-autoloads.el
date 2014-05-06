;;; mo-git-blame-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (mo-git-blame-current-for-revision mo-git-blame-current
;;;;;;  mo-git-blame-file) "mo-git-blame" "mo-git-blame.el" (21351
;;;;;;  61699 0 0))
;;; Generated autoloads from mo-git-blame.el

(autoload 'mo-git-blame-file "mo-git-blame" "\
Calls `git blame' for REVISION of FILE-NAME or `HEAD' if
REVISION is not given. Initializes the two windows that will show
the output of 'git blame' and the content.

If FILE-NAME is missing it will be read with `find-file' in
interactive mode.

ORIGINAL-FILE-NAME defaults to FILE-NAME if not given. This is
used for tracking renaming and moving of files during iterative
re-blaming.

With a numeric prefix argument or with NUM-LINES-TO-BLAME only
the NUM-LINES-TO-BLAME lines before and after point are blamed by
using git blame's `-L' option. Otherwise the whole file is
blamed.

\(fn &optional FILE-NAME REVISION ORIGINAL-FILE-NAME NUM-LINES-TO-BLAME)" t nil)

(autoload 'mo-git-blame-current "mo-git-blame" "\
Calls `mo-git-blame-file' for HEAD for the current buffer.

\(fn)" t nil)

(autoload 'mo-git-blame-current-for-revision "mo-git-blame" "\
Calls `mo-git-blame-file' for `revision' for the current buffer.

\(fn REVISION)" t nil)

;;;***

;;;### (autoloads nil nil ("mo-git-blame-pkg.el") (21351 61699 535377
;;;;;;  0))

;;;***

(provide 'mo-git-blame-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mo-git-blame-autoloads.el ends here
