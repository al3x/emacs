;;; ensime-core.el
;;
;;;; License
;;
;;     Copyright (C) 2010 Aemon Cannon
;;
;;     This file includes code from slime.el of the SLIME project
;;     (also licensend under the GNU General Public License.) The
;;     following copyrights therefore apply:
;;
;;     Copyright (C) 2003  Eric Marsden, Luke Gorrie, Helmut Eller
;;     Copyright (C) 2004,2005,2006  Luke Gorrie, Helmut Eller
;;     Copyright (C) 2007,2008,2009  Helmut Eller, Tobias C. Rittweiler
;;
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

;;; `ensime-rex' is the RPC primitive which is used to implement both
;;; `ensime-eval' and `ensime-eval-async'. You can use it directly if
;;; you need to, but the others are usually more convenient.

(defmacro* ensime-rex ((&rest saved-vars)
                       sexp
                       &rest continuations)
  "(ensime-rex (VAR ...) SEXP CLAUSES ...)

Remote EXecute SEXP.

VARs are a list of saved variables visible in the other forms.  Each
VAR is either a symbol or a list (VAR INIT-VALUE).

SEXP is evaluated and the princed version is sent to Lisp.

CLAUSES is a list of patterns with same syntax as
`destructure-case'.  The result of the evaluation of SEXP is
dispatched on CLAUSES.  The result is either a sexp of the
form (:ok VALUE) or (:abort REASON).  CLAUSES is executed
asynchronously.

Note: don't use backquote syntax for SEXP, because various Emacs
versions cannot deal with that."
  (let ((result (gensym)))
    `(lexical-let ,(loop for var in saved-vars
                         collect (etypecase var
                                   (symbol (list var var))
                                   (cons var)))
       (ensime-dispatch-event
        (list :swank-rpc ,sexp
              (lambda (,result)
                (destructure-case ,result
                                  ,@continuations)))))))

(put 'ensime-rex 'lisp-indent-function 2)

;;; Synchronous requests are implemented in terms of asynchronous
;;; ones. We make an asynchronous request with a continuation function
;;; that `throw's its result up to a `catch' and then enter a loop of
;;; handling I/O until that happens.

(defvar ensime-stack-eval-tags nil
  "List of stack-tags of continuations waiting on the stack.")

(defun ensime-eval (sexp)
  "Evaluate EXPR on the superior Lisp and return the result."
  (let* ((tag (gensym (format "ensime-result-%d-sym"
                              (1+ (ensime-continuation-counter)))))
         (ensime-stack-eval-tags (cons tag ensime-stack-eval-tags)))
    (apply
     #'funcall
     (catch tag
       (ensime-rex (tag sexp)
           sexp

         ((:ok value)
          (if (not (member tag ensime-stack-eval-tags))
              (message
               "Reply to canceled synchronous eval request tag=%S sexp=%S"
               tag sexp)
            (throw tag (list #'identity value))))

         ((:abort code reason)
          (message
           (format
            "Synchronous RPC Aborted: %s" reason))
          (throw tag (list #'identity nil))))

       (let ((debug-on-quit t)
             (inhibit-quit nil)
             (conn (ensime-connection)))
         (while t
           (unless (eq (process-status conn) 'open)
             (error "Lisp connection closed unexpectedly"))
           (accept-process-output nil 1 0)))))))


(defun ensime-eval-async (sexp &optional cont)
  "Evaluate EXPR on the superior Lisp and call CONT with the result."
  (ensime-rex (cont (buffer (current-buffer)))
      sexp
    ((:ok result)
     (when cont
       (if (buffer-live-p buffer)
           (progn
             (set-buffer buffer)
             (funcall cont result))
         (message
          "ENSIME: Asynchronous return could not find originating buffer.")
         )))
    ((:abort code reason)
     (message "Asynchronous RPC Aborted: %s" reason)))
  ;; Guard against arbitrary return values which once upon a time
  ;; showed up in the minibuffer spuriously (due to a bug in
  ;; ensime-autodoc.)  If this ever happens again, returning the
  ;; following will make debugging much easier:
  :ensime-eval-async)

(defun ensime-rpc-get-call-completion (id)
  (if (and (integerp id) (> id -1))
      (ensime-eval
       `(swank:call-completion ,id))))

(defun ensime-write-buffer (&optional filename clear-modtime set-unmodified)
  "Write the contents of buffer to its buffer-file-name.
Do not show 'Writing..' message."
  (let ((file (or filename buffer-file-name))
        (write-region-annotate-functions nil)
        (write-region-post-annotation-function nil))
    (when clear-modtime
      (clear-visited-file-modtime))
    (write-region (point-min) (point-max) file nil 'nomessage)
    (when set-unmodified
      (set-buffer-modified-p nil))
    ))

(defun ensime-rpc-completions-at-point (&optional max-results case-sens)
  (ensime-eval
   `(swank:completions
     ,buffer-file-name
     ,(ensime-computed-point)
     ,(or max-results 0)
     ,case-sens
     t ;; reload
     )))

(defun ensime-type-param-sections (type)
  (plist-get type :param-sections))

(defun ensime-in-string-or-comment (pos)
  "A helper to determine if the text at point is in a string
   or comment, and therefore should not be considered as part
   of a paren-balancing calculation.

   TODO: Currently this relies on font-lock-mode. Could be
   better."
  (let ((face (plist-get (text-properties-at pos) 'face)))
    (and face
     (or
      (equal face 'font-lock-doc-face)
      (equal face 'font-lock-string-face)
      (equal face 'font-lock-comment-face)))))

(defun ensime-type-name-with-args (type)
  (concat (plist-get type :name)
      (ensime-type-type-args-postfix type)))

(provide 'ensime-core)
