(defvar file-loadable-regexp
(replace-regexp-in-string
"\\." "\\\\."
(let (string
      (suffix-list (get-load-suffixes)))
  (concat (car suffix-list) "$"
          (dolist (extension (cdr suffix-list) string)
            (setq string (concat "\\|" extension "$" string))))))
"Regular expression that matches any file name with a file
extension returned by `get-load-suffixes'.")

(defun file-loadable-p (file)
"Return t if FILE is an Emacs lisp file.
More precisely, return t if the file name extension matches
`file-loadable-regexp'"
(string-match file-loadable-regexp file))

(defun load-directory (&optional directory recurse)
"Load all Emacs Lisp files in DIRECTORY.

Load files whose file name satisfies predicate `file-loadable-p'.
Non-interactively, DIRECTORY must be specified.  If both compiled
and uncompiled versions of the same file exist, only load the
compiled file.  If optional argument RECURSE is non-nil, (or,
interactively, with prefix argument) recursively load
subdirectories."
(interactive "P")
;; The idea here is to allow a prefix arg to specify recursion, but
;; also to read from the minibuffer the directory name; yet in
;; non-interactive use to only need the one directory-name argument,
;; as in: (load-directory "~/foo")
(let* ((recurse (if recurse recurse (when current-prefix-arg t)))
      (directory (if (stringp directory) directory
                   (when (called-interactively-p 'any)
                     (read-directory-name
                      (concat (if recurse "Recursively l" "L")
                              "oad all Emacs lisp files from directory: ")
                      default-directory default-directory t)))))
 ;; For non-interactive use
 (when (not (called-interactively-p 'any))
   (unless directory
     (error "Must specify a directory to when called non-interactively")))
 (unless (file-directory-p directory)
   (error "%s is not a directory" directory))
 (let ((file-list
        (directory-files (expand-file-name directory)
                         t directory-files-no-dot-files-regexp)))
   (dolist (file file-list)
     (cond
      ((and
        ;; This will include gzipped elisp files
        (file-loadable-p file)
        ;; Ignore symlinks to nonexistent targets.
        (file-exists-p file)
        ;; Don't try to load directies whose names end in ".el"
        ;; etc., as if they were files.  Note that we do not
        ;; predicate on "permission denied" problems, instead
        ;; letting things fail in that case so the user knows.
        (not (file-directory-p file))
        ;; If there exist both compiled and uncompiled versions of
        ;; the same library, only load the compiled one.  (This is
        ;; why we let-bind the `file-list'.)  This could perhaps be
        ;; factored out, and currently still double-loads gzipped
        ;; libraries.
        (not (and (string= (file-name-extension file t) ".el")
                  (member
                   (concat (file-name-sans-extension file) ".elc")
                   file-list))))
       (load file))
      ((and (file-directory-p file)
            recurse)
       (load-directory file t)))))))