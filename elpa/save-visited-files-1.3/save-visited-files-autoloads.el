;;; save-visited-files-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (turn-off-save-visited-files-mode turn-on-save-visited-files-mode
;;;;;;  save-visited-files-mode save-visited-files-restore save-visited-files-save)
;;;;;;  "save-visited-files" "save-visited-files.el" (20456 45764))
;;; Generated autoloads from save-visited-files.el

(autoload 'save-visited-files-save "save-visited-files" "\
Save the list of currently visited files

\(fn &optional LOCATION)" t nil)

(autoload 'save-visited-files-restore "save-visited-files" "\
Restore all files that were saved by save-visited-files-save.

\(fn &optional LOCATION)" t nil)

(defvar save-visited-files-mode nil "\
Non-nil if Save-Visited-Files mode is enabled.
See the command `save-visited-files-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `save-visited-files-mode'.")

(custom-autoload 'save-visited-files-mode "save-visited-files" nil)

(autoload 'save-visited-files-mode "save-visited-files" "\
Minor mode to automatically save a list of all open files, and
optionally open all files from such a list at startup.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-save-visited-files-mode "save-visited-files" "\
Turns save-visited-files-mode on

\(fn)" t nil)

(autoload 'turn-off-save-visited-files-mode "save-visited-files" "\
Turns save-visited-files-mode off

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("save-visited-files-pkg.el") (20456 45764
;;;;;;  663820))

;;;***

(provide 'save-visited-files-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; save-visited-files-autoloads.el ends here
