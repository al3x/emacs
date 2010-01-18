;;; gdb-shell.el --- minor mode to add gdb features to shell

;; Copyright (C) 2007, 2009 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 17 Apr 2007
;; Version: 0.4
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This minor mode will recognize various commands in shell mode and
;; take one of two actions.  For "build" commands (currently make,
;; valgrind, and ant), it will enable compilation-shell-minor-mode.
;; For "gdb", it will invoke Emacs' built-in gdb rather than running
;; it in the shell buffer.  This is especially handy if you use
;; "gdb --args".

;;; Code:

(defconst gdb-shell-gdb-regexp "^\\(gdb\\)\\( .*\\)$")

;; no need for this, just run compilation-shell-minor-mode
;; yourself.  duh.
(defconst gdb-shell-make-regexp "^\\(make\\|valgrind\\|ant\\) ")

(defun gdb-shell-input-sender (proc string)
  (save-match-data
    (cond
     ((string-match gdb-shell-gdb-regexp string)
      ;; We require gud here, so that our let-binding a local does
      ;; not shadow a defvar, causing problems later.
      (require 'gud)
      (let ((gud-chdir-before-run nil))
	(if (boundp 'gud-gdb-command-name)
	    ;; Emacs 22.
	    (setq string (concat gud-gdb-command-name
				 (match-string 2 string)))
	  ;; Emacs 21.
	  (setq string (concat (match-string 1 string)
			       ;; We could use -cd but there doesn't
			       ;; seem to be a reason to.
			       " -fullname"
			       (match-string 2 string))))
	;; We only need this for Emacs 21, but it is simpler to
	;; always do it.
	(flet ((gud-gdb-massage-args (file args) args))
	  (gdb string))
	(setq string "")))
     ((string-match gdb-shell-make-regexp string)
      (compilation-shell-minor-mode 1))))
  (comint-simple-send proc string))

;;;###autoload
(define-minor-mode gdb-shell-minor-mode
  "Minor mode to add gdb features to shell mode."
  nil
  ""
  nil
  (if gdb-shell-minor-mode
      (progn
	(make-local-variable 'comint-input-sender)
	(setq comint-input-sender 'gdb-shell-input-sender))
    (setq comint-input-sender 'comint-simple-send)))

(provide 'gdb-shell)

;;; gdb-shell.el ends here
