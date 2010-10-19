;;; ensime-builder.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.


(defvar ensime-builder-result-buffer-name "*ENSIME-Builder*")

(defvar ensime-builder-result-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda()(interactive)
				(ensime-popup-buffer-quit-function)
				))
    (define-key map [?\t] 'forward-button)
    (define-key map [mouse-1] 'push-button)
    map)
  "Key bindings for the build result popup.")

(defface ensime-builder-errline
  '((((class color) (background dark)) (:foreground "#ff5555"))
    (((class color) (background light)) (:foreground "Firebrick4"))
    (t (:bold t)))
  "Face used for marking the line on which an error occurs."
  :group 'ensime-ui)

(defface ensime-builder-warnline
  '((((class color) (background dark)) (:foreground "LightBlue2"))
    (((class color) (background light)) (:foreground "DarkBlue"))
    (t (:bold t)))
  "Face used for marking the line on which an warning occurs."
  :group 'ensime-ui)

(defun ensime-builder-show-result-buffer (notes)
  "Show a popup listing the results of the last build."
  (ensime-with-popup-buffer
   (ensime-refactor-info-buffer-name t t)
   (use-local-map ensime-builder-result-map)
   (ensime-insert-with-face 
    "Result of Incremental Build (q to quit, TAB to jump to next error)"
    'font-lock-constant-face)
   (ensime-insert-with-face 
    "\n----------------------------------------\n\n"
    'font-lock-comment-face)
   (if (null notes)
       (insert "Finished with 0 errors, 0 warnings.")
     (save-excursion
       (dolist (note notes)
	 (destructuring-bind 
	     (&key severity msg beg end line col file &allow-other-keys) note
	   (let ((face (case severity
			 (error 'ensime-builder-errline)
			 (warn 'ensime-builder-warnline)
			 (info font-lock-string-face)
			 (otherwise font-lock-comment-face)
			 ))
		 (header (case severity
			   (error "ERROR")
			   (warn "WARNING")
			   (info "INFO")
			   (otherwise "MISC")
			   ))
		 (p (point)))
	     (insert (format "%s: %s : %s : %s" header msg file line))
	     (ensime-make-code-link p (point) file (+ 1 beg) face)))
	 (insert "\n\n")))
     )))


(defun ensime-builder-build ()
  "Start the incremental builder. This command will trigger
a full recompile of the entire project!"
  (interactive)
  (setf (ensime-builder-changed-files (ensime-connection)) nil)
  (ensime-rpc-async-builder-init 'ensime-builder-show-result-buffer))

(defun ensime-builder-track-changed-files ()
  "Invoked when an ENSIME source buffer is saved. Store the filename
with all others that have been saved(modified) since the last rebuild."
  (when (and (ensime-connected-p) ;; don't want an error in a hook
	     buffer-file-name 
	     (file-exists-p buffer-file-name))
    (let ((changed-files (ensime-builder-changed-files 
			  (ensime-connection))))
      (when (not (memq buffer-file-name changed-files))
	(push buffer-file-name (ensime-builder-changed-files
				(ensime-connection)))))))

(defun ensime-builder-rebuild ()
  "Send a request for rebuild to the ENSIME server. Sends filenames of
all files that have been changed since the last rebuild, so incremental
builder can avoid extra work."
  (interactive)
  (let ((change-set (ensime-builder-changed-files 
		     (ensime-connection))))
    (if change-set
	(progn
	  (ensime-rpc-async-builder-update 
	   change-set 
	   'ensime-builder-show-result-buffer)
	  (setf (ensime-builder-changed-files (ensime-connection)) nil))
      (message "Nothing to rebuild."))))

(provide 'ensime-builder)