;;; ensime-connections.el
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


;;;; Connections
;;;
;;; "Connections" are the high-level Emacs<->ENSIME-Server networking concept.
;;;
;;; Emacs has a connection to each ENSIME server process that it's interacting
;;; with. Typically there would only be one, but a user can choose to
;;; connect to many Servers simultaneously.
;;;
;;; A connection consists of a control socket and a
;;; set of connection-local state variables.
;;;
;;; The state variables are stored as buffer-local variables in the
;;; control socket's process-buffer and are used via accessor
;;; functions. These variables include things like the *FEATURES* list
;;; and Unix Pid of the Server process.
;;;
;;; One connection is "current" at any given time. This is:
;;;   `ensime-dispatching-connection' if dynamically bound, or
;;;   `ensime-buffer-connection' if this is set buffer-local,
;;;   or the value of `(ensime-owning-connection-for-source-file buffer-file-name)'
;;;   otherwise.
;;;
;;; When you're invoking commands in your source files you'll be using
;;; `(ensime-owning-connection-for-source-file)'.
;;;
;;; When a command creates a new buffer it will set
;;; `ensime-buffer-connection' so that commands in the new buffer will
;;; use the connection that the buffer originated from. For example,
;;; the apropos command creates the *Apropos* buffer and any command
;;; in that buffer (e.g. `M-.') will go to the same Lisp that did the
;;; apropos search. REPL buffers are similarly tied to their
;;; respective connections.
;;;
;;; When Emacs is dispatching some network message that arrived from a
;;; connection it will dynamically bind `ensime-dispatching-connection'
;;; so that the event will be processed in the context of that
;;; connection.
;;;
;;; This is mostly transparent. The user should be aware that he can
;;; set the default connection to pick which Server handles commands in
;;; ensime-mode source buffers, and ensime hackers should be aware that
;;; they can tie a buffer to a specific connection. The rest takes
;;; care of itself.


(defmacro ensime-def-connection-var (varname &rest initial-value-and-doc)
  "Define a connection-local variable.
The value of the variable can be read by calling the function of the
same name (it must not be accessed directly). The accessor function is
setf-able.

The actual variable bindings are stored buffer-local in the
process-buffers of connections. The accessor function refers to
the binding for `ensime-connection'."
  (let ((real-var (intern (format "%s:connlocal" varname))))
    `(progn
       ;; Variable
       (make-variable-buffer-local
    (defvar ,real-var ,@initial-value-and-doc))
       ;; Accessor
       (defun ,varname (&optional process)
     (ensime-with-connection-buffer (process) ,real-var))
       ;; Setf
       (defsetf ,varname (&optional process) (store)
     `(ensime-with-connection-buffer
       (,process)
       (setq (\, (quote (\, real-var))) (\, store))
       (\, store)))
       '(\, varname))))

(put 'ensime-def-connection-var 'lisp-indent-function 2)
(put 'ensime-indulge-pretty-colors 'ensime-def-connection-var t)

(ensime-def-connection-var ensime-connection-number nil
  "Serial number of a connection.
Bound in the connection's process-buffer.")

(ensime-def-connection-var ensime-server-features '()
  "The symbol-names of Lisp's *FEATURES*.
This is automatically synchronized from Lisp.")

(ensime-def-connection-var ensime-pid nil
  "The process id of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-type nil
  "The implementation type of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-version nil
  "The implementation type of the Lisp process.")

(ensime-def-connection-var ensime-server-implementation-name nil
  "The short name for the Lisp implementation.")

(ensime-def-connection-var ensime-server-implementation-program nil
  "The argv[0] of the process running the Lisp implementation.")

(ensime-def-connection-var ensime-connection-name nil
  "The short name for connection.")

(ensime-def-connection-var ensime-server-process nil
  "The inferior process for the connection if any.")

(ensime-def-connection-var ensime-config nil
  "The project configuration corresponding to this connection.")

(ensime-def-connection-var ensime-communication-style nil
  "The communication style.")

(ensime-def-connection-var ensime-machine-instance nil
  "The name of the (remote) machine running the Lisp process.")

(ensime-def-connection-var ensime-analyzer-ready nil
  "Whether the analyzer has finished its initial run.")

(ensime-def-connection-var ensime-scala-compiler-notes nil
  "Warnings, Errors, and other notes produced by the analyzer.")

(ensime-def-connection-var ensime-java-compiler-notes nil
  "Warnings, Errors, and other notes produced by the analyzer.")

(ensime-def-connection-var ensime-builder-changed-files nil
  "Files that have changed since the last rebuild.")

(ensime-def-connection-var ensime-awaiting-full-typecheck nil
  "Should we show the errors and warnings report on next full-typecheck event?")

(ensime-def-connection-var ensime-num-errors 0
  "Current number of errors in project.")

(ensime-def-connection-var ensime-num-warnings 0
  "Current number of warnings in project.")

(defvar ensime-dispatching-connection nil
  "Network process currently executing.
This is dynamically bound while handling messages from Lisp; it
overrides `ensime-buffer-connection'.")

(make-variable-buffer-local
 (defvar ensime-buffer-connection nil
   "Network connection to use in the current buffer."))


(defvar ensime-connection-counter 0
  "The number of ENSIME connections made. For generating serial numbers.")

(defun ensime-current-connection ()
  "Return the connection to use for Lisp interaction.
 Return nil if there's no connection. Note, there is some loss of
 precision here, as ensime-connections-for-source-file might return
 more than one connection. "
  (or (ensime-validated-connection ensime-dispatching-connection)
      (ensime-validated-connection ensime-buffer-connection)
      (ensime-validated-connection
       (ensime-owning-connection-for-source-file buffer-file-name))))

(defun ensime-validated-connection (conn)
  "Return conn if connection is non-nil and has a living
 process buffer. nil otherwise."
  (when (and conn (buffer-live-p (process-buffer conn)))
    conn))

(defun ensime-connected-p (&optional conn)
  "Return t if ensime-current-connection would return non-nil.
 Return nil otherwise."
  (let ((conn (or conn (ensime-current-connection))))
    (and conn
     (buffer-live-p (process-buffer conn)))))


(defun ensime-connection ()
  "Return the connection to use for Lisp interaction.
 Signal an error if there's no connection."
  (let ((conn (ensime-current-connection)))
    (cond ((not conn)
           (or (ensime-auto-connect)
               (error "Not connected. M-x ensime to connect")))
          ((not (eq (process-status conn) 'open))
           (error "Connection closed."))
          (t conn))))


(defun ensime-connection-visiting-buffers (conn)
  "Return a list of all buffers associated with the given
 connection."
  (let ((result '()))
    (dolist (buf (buffer-list))
      (let ((f (buffer-file-name buf)))
        (when (and f (ensime-file-belongs-to-connection-p f conn))
          (setq result (cons buf result)))))
    result))


(defun ensime-file-belongs-to-connection-p (file-in conn)
  "Does the given file belong to the given connection(project)?"
  (let* ((file (file-truename file-in))
         (config (ensime-config conn))
         (source-roots (plist-get config :source-roots)))
    (catch 'return
      (dolist (dir source-roots)
        (when (ensime-file-in-directory-p file dir)
          (throw 'return t))))))


(defun ensime-connections-for-source-file (file-in)
  "Return the connections corresponding to projects that contain
   the given file in their source trees."
  (let ((file (file-truename file-in)))
    (when file
      (let ((result '()))
        (dolist (conn ensime-net-processes)
          (when-let (conn (ensime-validated-connection conn))
                    (let* ((config (ensime-config conn))
                           (source-roots (plist-get config :source-roots)))
                      (dolist (dir source-roots)
                        (when (ensime-file-in-directory-p file dir)
                          (setq result (cons conn result)))))))
        result))))

(defun ensime-probable-owning-connection-for-source-file
  (file-in)
  (ensime-owning-connection-for-source-file file-in t))

(defun ensime-owning-connection-for-source-file (file-in &optional loose)
  "Return the connection corresponding to the single that
 owns the given file. "
  (when file-in
    (let ((file (file-truename file-in)))
      (when file
        (catch 'return
          ;; First check individual source-roots
          (dolist (conn ensime-net-processes)
            (when-let (conn (ensime-validated-connection conn))
                      (let* ((config (ensime-config conn))
                             (project-root (plist-get config :root-dir))
                             (source-roots (plist-get config :source-roots)))
                        (if (and loose (ensime-file-in-directory-p file project-root))
                            (throw 'return conn)
                          (dolist (dir source-roots)
                            (when (ensime-file-in-directory-p file dir)
                              (throw 'return conn)))))))
          )))))



(defun ensime-prompt-for-connection ()
  "Prompt the user to select a server connection. Used in situations where
the active connection is ambiguous."
  (let* ((options
      (mapcar
       (lambda (p)
         (let* ((conf (ensime-config p))
            (root (plist-get conf :root-dir))
            (num (ensime-connection-number p)))
           `(,(format "%s#%s" root num) . ,p)))
       ensime-net-processes))
     (keys (mapcar (lambda (opt) (car opt)) options)))
    (let ((key (when keys
         (completing-read
          (concat "Which project to use? ("
              (mapconcat #'identity keys ", ")
              "): ")
          keys nil t (car keys)))))
      (cdr (assoc key options)))))


;; FIXME: should be called auto-start
(defcustom ensime-auto-connect 'never
  "Controls auto connection when information from lisp process is needed.
This doesn't mean it will connect right after Ensime is loaded."
  :group 'ensime-mode
  :type '(choice (const never)
         (const always)
         (const ask)))

(defun ensime-auto-connect ()
  (cond ((or (eq ensime-auto-connect 'always)
         (and (eq ensime-auto-connect 'ask)
          (y-or-n-p "No connection.  Start Ensime? ")))
     (save-window-excursion
       (ensime)
       (while (not (ensime-current-connection))
         (sleep-for 1))
       (ensime-connection)))
    (t nil)))

(defun ensime-setup-connection (process)
  "Make a connection out of PROCESS."
  (let ((ensime-dispatching-connection process))

    ;; Initialize connection state in the process-buffer of PROC."

    ;; To make life simpler for the user: if this is the only open
    ;; connection then reset the connection counter.
    (when (equal ensime-net-processes (list process))
      (setq ensime-connection-counter 0))

    (ensime-with-connection-buffer
     () (setq ensime-buffer-connection process))

    (setf (ensime-connection-number process)
      (incf ensime-connection-counter))

    process))

(defun ensime-connect (host port)
  "Connect to a running Swank server. Return the connection."
  (interactive (list
        (read-from-minibuffer "Host: " ensime-default-server-host)
        (read-from-minibuffer "Port: " (format "%d" ensime-default-port)
                      nil t)))
  (when (and (interactive-p) ensime-net-processes
         (y-or-n-p "Close old connections first? "))
    (ensime-disconnect-all))
  (message "Connecting to Swank on port %S.." port)
  (let ()
    (message "Connecting to Swank on port %S.." port)
    (let* ((process (ensime-net-connect host port))
       (ensime-dispatching-connection process))
      (ensime-setup-connection process))))




(defun ensime-handle-connection-info (connection info)
  "Initialize CONNECTION with INFO received from Lisp."
  (ensime-event-sig :connected info)
  (let ((ensime-dispatching-connection connection))
    (destructuring-bind (&key pid server-implementation version
                              &allow-other-keys) info
      (setf (ensime-pid) pid)
      (destructuring-bind (&key name) server-implementation
        (setf (ensime-server-implementation-name) name
              (ensime-connection-name) (ensime-generate-connection-name name)))
      ))

  (run-hooks 'ensime-connected-hook)
  (message "Connected.")

  ;; Send the project initialization..
  (let ((config (ensime-config connection)))
    (ensime-init-project connection config))

  )

(provide 'ensime-connections)
