;;; ensime-macros.el
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

(eval-when-compile (require 'cl))

(defmacro ensime-with-conn-interactive (conn-sym &rest body)
  "Surround body forms with a check to see if we're connected.
If not, message the user."
  `(let* ((,conn-sym (or (ensime-current-connection)
			 (ensime-prompt-for-connection))))
     (if conn
	 (progn ,@body)
       (message
	"This command requires a connection to an ENSIME server."))))

(defmacro* when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.

\(fn (VAR VALUE) &rest BODY)"
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro* ensime-with-popup-buffer ((name &optional connection select)
				     &body body)
  "Similar to `with-output-to-temp-buffer'.
Bind standard-output and initialize some buffer-local variables.
Restore window configuration when closed.

NAME is the name of the buffer to be created.
CONNECTION is the value for `ensime-buffer-connection'.
If nil, no explicit connection is associated with
the buffer.  If t, the current connection is taken.
"
  `(let* ((vars% (list ,(if (eq connection t) '(ensime-connection) connection)))
	  (standard-output (ensime-make-popup-buffer ,name vars%)))
     (with-current-buffer standard-output
       (prog1
	   (progn
	     ,@body)
	 (assert (eq (current-buffer) standard-output))
	 (setq buffer-read-only t)
	 (set-window-point (ensime-display-popup-buffer ,(or select 'nil))
			   (point))))))


(provide 'ensime-macros)
