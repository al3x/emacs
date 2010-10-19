;;; ensime-test.el --- Regression tests for ENSIME
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


(require 'ensime)

(defvar ensime-testing-buffer "*ensime-tests*"
  "Contains the output of all the tests. Also tracks all the testing-specific
   buffer-local variables.")

(defvar ensime-test-queue '()
  "The queue of tests yet to be run.")
(make-variable-buffer-local 'ensime-test-queue)

(defvar ensime-async-handler-stack '()
  "Asynchronous event handlers waiting for signals. See 'ensime-test-sig'.")
(make-variable-buffer-local 'ensime-async-handler-stack)

(defvar ensime-shared-test-state '()
  "A state dump for anyone who wants to use it. Useful for async tests.")
(make-variable-buffer-local 'ensime-shared-test-state)

(defvar ensime-test-env-classpath '("/home/aemon/lib/scala/lib/scala-library.jar")
  "Hard-code a classpath for testing purposes. Not great.")

(put 'ensime-test-assert-failed 'error-conditions '(error ensime-test-assert-failed))
(put 'ensime-test-assert-failed 'error-message "Assertion Failed")

(put 'ensime-test-interrupted 'error-conditions '(error ensime-test-interrupted))
(put 'ensime-test-interrupted 'error-message "Test Interrupted")


(defun ensime-test-concat-lines (&rest lines)
  (mapconcat #'identity lines "\n"))


(defun ensime-create-file (file-name contents)
  "Create file named file-name. Write contents to the file. Return file's name."
  (with-temp-file file-name
    (insert contents))
  file-name)


(defmacro* ensime-with-tmp-file ((name prefix contents) &rest body)
  "Create temporary file with given prefix. Bind the file to
   name and evaluate body."
  `(let ((,name (make-temp-file ,prefix)))
     (with-temp-file ,name
       (insert ,contents))
     (unwind-protect
         (progn ,@body)
       (delete-file ,name))))



(defun ensime-create-tmp-project (src-files)
  "Create a temporary project directory. Populate with config, source files. Return
   a plist describing the project. Note: Delete such projects with
   ensime-cleanup-tmp-project."
  (let* ((root-dir (file-name-as-directory (make-temp-file "ensime_test_proj_" t)))
         (conf-file (ensime-create-file
                     (concat root-dir ".ensime")
                     (format "%S" (list :sources '("src")
                                        :project-package "com.test"
                                        :dependency-jars ensime-test-env-classpath
                                        ))))
         (src-dir (file-name-as-directory (concat root-dir "src"))))
    (mkdir src-dir)
    (let* ((proj '())
           (src-file-names
            (mapcar
             (lambda (f) (ensime-create-file
                          (concat src-dir (plist-get f :name))
                          (plist-get f :contents)))
             src-files)))
      (setq proj (plist-put proj :src-files src-file-names))
      (setq proj (plist-put proj :root-dir root-dir))
      (setq proj (plist-put proj :conf-file conf-file))
      (setq proj (plist-put proj :src-dir src-dir))
      proj
      )))

(defvar ensime-tmp-project-hello-world
  `((:name
     "hello_world.scala"
     :contents ,(ensime-test-concat-lines
                 "package com.helloworld"
                 "class HelloWorld{"
                 "}"
                 "object HelloWorld {"
                 "def main(args: Array[String]) = {"
                 "Console.println(\"Hello, world!\")"
                 "}"
                 "def foo(a:Int, b:Int):Int = {"
                 "a + b"
                 "}"
                 "}"
                 )
     )))

(defun ensime-cleanup-tmp-project (proj)
  "Destroy a temporary project directory, kill all buffers visiting
   source files in the project."
  (let ((src-files (plist-get proj :src-files))
        (root-dir (plist-get proj :root-dir)))
    (dolist (f src-files)
      (find-file f)
      (set-buffer-modified-p nil)
      (kill-buffer nil))
    ;; a bit of paranoia..
    (if (and root-dir (integerp (string-match "^/tmp/" root-dir)))
        ;; ..before we wipe away the project dir
        (shell-command (format "rm -rf %S" root-dir)))))

(defun ensime-kill-all-ensime-servers ()
  "Kill all inferior ensime server buffers."
  (dolist (b (buffer-list))
    (if (string-match "^\\*inferior-ensime-server" (buffer-name b))
        (kill-buffer b))))

(defmacro ensime-test-var-put (var val)
  "Helper for writing to shared testing state."
  `(with-current-buffer ensime-testing-buffer
     (setq ensime-shared-test-state
           (plist-put ensime-shared-test-state ,var ,val))))

(defmacro ensime-test-var-get (var)
  "Helper for reading from shared testing state."
  `(with-current-buffer ensime-testing-buffer
     (plist-get ensime-shared-test-state ,var)))

(defun ensime-test-sig (event value)
  "Driver for asynchonous tests. This function is invoked from ensime core,
   signaling events to events handlers installed by asynchronous tests."
  (when (buffer-live-p (get-buffer ensime-testing-buffer))
    (with-current-buffer ensime-testing-buffer
      (when (not (null ensime-async-handler-stack))
        (let* ((ensime-prefer-noninteractive t)
               (handler (car ensime-async-handler-stack))
               (handler-event (plist-get handler :event)))
          (when (equal event handler-event)
            (let ((handler-func (plist-get handler :func))
                  (is-last (plist-get handler :is-last)))
              (pop ensime-async-handler-stack)
              (save-excursion
                (condition-case signal
                    (funcall handler-func value)
                  (ensime-test-interrupted
                   (message "Error executing test, moving to next.")
                   (setq is-last t))))
              (when is-last
                (setq ensime-async-handler-stack nil)
                (pop ensime-test-queue)
                (ensime-run-next-test)))))))))


(defun ensime-test-output (txt)
  "Helper for writing text to testing buffer."
  (with-current-buffer ensime-testing-buffer
    (goto-char (point-max))
    (insert (format "\n%s" txt))))

(defun ensime-test-output-result (result)
  "Helper for writing results to testing buffer."
  (if (equal result t)
      (ensime-test-output (format "OK" ))
    (ensime-test-output (format "%s\n" result))))


(defmacro ensime-test-suite (&rest tests)
  "Define a sequence of tests to execute.
   Tests may be synchronous or asynchronous."
  `(progn
     (switch-to-buffer ensime-testing-buffer)
     (erase-buffer)
     (setq ensime-test-queue (list ,@tests))
     (ensime-run-next-test)))


(defmacro ensime-test (title &rest body)
  "Define a synchronous test."
  `(list :title ,title :async nil
         :func (lambda ()
                 (ensime-test-run-with-handlers
                  ,title
                  ,@body))))


(defmacro ensime-test-run-with-handlers (context &rest body)
  "Evaluate body in the context of an error handler. Handle errors by
   writing to the testing output buffer."
  `(save-excursion
     (condition-case signal
         (progn
           ,@body
           (ensime-test-output-result t))
       (ensime-test-assert-failed
        (ensime-test-output-result
         (format "Assertion failed at '%s': %s" ,context signal))
        (signal 'ensime-test-interrupted (format "Test interrupted."))
        ))))


(defmacro* ensime-async-test (title trigger &rest handlers)
  "Define an asynchronous test."
  (let* ((last-handler (car (last handlers)))
         (handler-structs
          (mapcar
           (lambda (h)
             (let* ((head (car h))
                    (evt (car head))
                    (val-sym (cadr head))
                    (func-body (cadr h))
                    (func `(lambda (,val-sym)
                             (ensime-test-run-with-handlers
                              ,title
                              ,func-body))))
               (list
                :event evt
                :val-sym val-sym
                :func func
                :is-last (equal h last-handler)
                )))
           handlers))
         (trigger-func
          `(lambda ()
             (ensime-test-run-with-handlers
              ,title
              ,trigger))))
    `(list :title ,title :async t
           :trigger ,trigger-func
           :handlers ',handler-structs
           )))

;;(message "%S" (macroexpand '(ensime-async-test "blarg" (do-this-thing) ((:evt-a val) val) ((:evt-b val) val))))


(defun ensime-run-next-test ()
  "Run the next test from the test queue."
  (with-current-buffer ensime-testing-buffer
    (if ensime-test-queue
        (let ((ensime-prefer-noninteractive t)
              (test (car ensime-test-queue)))
          (setq ensime-shared-test-state '())
          (setq ensime-async-handler-stack '())

          (ensime-test-output (format "\n%s" (plist-get test :title)))

          (if (plist-get test :async)

              ;; Asynchronous test
              (let ((handlers (reverse (plist-get test :handlers))))
                (dolist (h handlers)
                  (push h ensime-async-handler-stack))
                (funcall (plist-get test :trigger)))

            ;; Synchronous test
            (progn
              (pop ensime-test-queue)
              (save-excursion
                (condition-case signal
                    (funcall (plist-get test :func))
                  (ensime-test-interrupted
                   (message "Error executing test, moving to next."))))
              (ensime-run-next-test))))
      (goto-char (point-max))
      (insert "\n\nFinished.")
      )))


(defmacro ensime-assert (pred)
  `(let ((val ,pred))
     (if (not val)
         (with-current-buffer ensime-testing-buffer
           (signal 'ensime-test-assert-failed (format "Expected truth of %s." ',pred))))))


(defmacro ensime-assert-equal (a b)
  `(let ((val-a ,a)
         (val-b ,b))
     (if (equal val-a val-b) t
       (with-current-buffer ensime-testing-buffer
         (signal 'ensime-test-assert-failed (format "Expected %s to equal %s." ',a ',b))))))

(defun ensime-stop-tests ()
  "Forcibly stop all tests in progress."
  (interactive)
  (with-current-buffer ensime-testing-buffer
    (setq ensime-async-handler-stack nil)
    (setq ensime-test-queue nil))
  (switch-to-buffer ensime-testing-buffer))

(defun ensime-test-eat-mark (mark)
  (goto-char (point-min))
  (when (search-forward-regexp (concat "/\\*" mark "\\*/") nil t)
    (kill-backward-chars (+ 4 (length mark)))))

;;;;;;;;;;;;;;;;;;
;; ENSIME Tests ;;
;;;;;;;;;;;;;;;;;;

(defun ensime-run-fast-tests ()
  (interactive)
  (ensime-test-suite


   (ensime-test
    "Test loading a simple config."
    (ensime-with-tmp-file
     (file "ensime_test_conf_"
           (format "%S"
                   '( :server-cmd
                      "bin/server.sh"
                      :dependendency-dirs ("hello" "world")
                      )))
     (let ((conf (ensime-config-load file)))
       (ensime-assert (equal (plist-get conf :server-cmd) "bin/server.sh"))
       (ensime-assert (equal (plist-get conf :dependendency-dirs) '("hello" "world")))
       (ensime-assert (equal (plist-get conf :root-dir)
                             (expand-file-name (file-name-directory file)))))))


   (ensime-test
    "Test loading a broken(syntactically) config file."
    (ensime-with-tmp-file
     (file "ensime_test_conf_" "(lkjsdfkjskfjs")
     (let ((conf
            (condition-case er
                (ensime-config-load file)
              (error nil))))
       (ensime-assert (null conf)))))

   (ensime-test
    "Test name partitioning..."

    (ensime-with-name-parts
     "java.util.List" (p o n)
     (ensime-assert-equal (list p o n)
                          (list "java.util" nil "List")))

    (ensime-with-name-parts
     "scala.tools.nsc.symtab.Types$Type" (p o n)
     (ensime-assert-equal (list p o n)
                          (list "scala.tools.nsc.symtab" "Types" "Type")))

    (ensime-with-name-parts
     "scala.tools.nsc.symtab.Types" (p o n)
     (ensime-assert-equal (list p o n)
                          (list "scala.tools.nsc.symtab" nil "Types")))

    (ensime-with-name-parts
     "scala.tools.nsc.symtab.Types$Dude$AbsType" (p o n)
     (ensime-assert-equal (list p o n)
                          (list "scala.tools.nsc.symtab" "Types$Dude" "AbsType")))

    (ensime-with-name-parts
     "scala.tools.nsc.symtab.Types$$Type$" (p o n)
     (ensime-assert-equal (list p o n)
                          (list "scala.tools.nsc.symtab" "Types$" "Type$")))

    (ensime-with-name-parts
     "Types$$Type$" (p o n)
     (ensime-assert-equal (list p o n)
                          (list "" "Types$" "Type$")))

    )

   (ensime-test
    "Test course name partitioning..."

    (ensime-with-path-and-name
     "java.util.List" (p n)
     (ensime-assert-equal (list p n)
                          (list "java.util" "List")))

    (ensime-with-path-and-name
     "scala.tools.nsc.symtab.Types$Type" (p n)
     (ensime-assert-equal (list p n)
                          (list "scala.tools.nsc.symtab" "Types$Type")))

    (ensime-with-path-and-name
     "scala.tools.nsc.symtab.Types" (p n)
     (ensime-assert-equal (list p n)
                          (list "scala.tools.nsc.symtab" "Types")))

    (ensime-with-path-and-name
     "scala.tools.nsc.symtab.Types$Dude$AbsType" (p n)
     (ensime-assert-equal (list p n)
                          (list "scala.tools.nsc.symtab" "Types$Dude$AbsType")))

    (ensime-with-path-and-name
     "scala.tools.nsc.symtab.Types$$Type$" (p n)
     (ensime-assert-equal (list p n)
                          (list "scala.tools.nsc.symtab" "Types$$Type$")))

    (ensime-with-path-and-name
     "Types$$Type$" (p n)
     (ensime-assert-equal (list p n)
                          (list "" "Types$$Type$")))

    (ensime-with-path-and-name
     "java.uti" (p n)
     (ensime-assert-equal (list p n)
                          (list "java" "uti")))

    (ensime-with-path-and-name
     "uti" (p n)
     (ensime-assert-equal (list p n)
                          (list "" "uti")))

    )


   (ensime-test
    "Test is source file predicate..."
    (ensime-assert (ensime-is-source-file-p "dude.scala"))
    (ensime-assert (ensime-is-source-file-p "dude.java"))
    (ensime-assert (not (ensime-is-source-file-p "dude.javap"))))

   (ensime-test
    "Test relativization of paths..."
    (ensime-assert-equal
     "./rabbits.txt" (ensime-relativise-path "/home/aemon/rabbits.txt" "/home/aemon/"))
    (ensime-assert-equal
     "./a/b/d.txt" (ensime-relativise-path "/home/aemon/a/b/d.txt" "/home/aemon/" ))
    (ensime-assert-equal
     "./a/b/d.txt" (ensime-relativise-path  "c:/home/aemon/a/b/d.txt" "c:/home/aemon/"))
    (ensime-assert-equal
     "c:/home/blamon/a/b/d.txt" (ensime-relativise-path  "c:/home/blamon/a/b/d.txt" "c:/home/aemon/")))

   ))


(defun ensime-run-slow-tests ()
  (interactive)
  (ensime-test-suite


   (ensime-async-test
    "Test completing members."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"

                                 "class HelloWorld{"
                                 "  def foo(a:Int, b:Int):Int = {"
                                 "    HelloWorld./*1*/"
                                 "  }"
                                 "  def bar(a:Int, b:Int):Int = {"
                                 "    val v = HelloWorld./*2*/"
                                 "    foo(1,v)"
                                 "  }"
                                 "}"

                                 "object HelloWorld {"
                                 "  def blarg = 5"
                                 "  def add(a:Int, b:Int) = {"
                                 "    System.out.pri/*3*/"
                                 "    a + b"
                                 "  }"
                                 "  def test() {"
                                 "    val dude = \"hello\""
                                 "    System.out.println(dude./*4*/)"
                                 "  }"
                                 "  def test2() = {"
                                 "    val dude = \"hello\""
                                 "    dude.substring(2,2).hea/*5*/"
                                 "  }"
                                 "}"
                                 )
                     ))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))
       ;; Set cursor to symbol in method body..
       (find-file (car src-files))

       ;; object method completion
       (ensime-test-eat-mark "1")
       (let* ((candidates (ensime-ac-member-candidates "")))
         (ensime-assert (member "add" candidates)))

       ;; Try completion when a method begins without target
       ;; on next line.
       (ensime-test-eat-mark "2")
       (let* ((candidates (ensime-ac-member-candidates "")))
         (ensime-assert (member "blarg" candidates)))

       ;; Instance completion with prefix
       (ensime-test-eat-mark "3")
       (let* ((candidates (ensime-ac-member-candidates "pri")))
         (ensime-assert (member "println" candidates)))

       ;; Complete member of argument
       (ensime-test-eat-mark "4")
       (let* ((candidates (ensime-ac-member-candidates "s")))
         (ensime-assert (member "substring" candidates)))

       ;; Chaining of calls
       (ensime-test-eat-mark "5")
       (let* ((candidates (ensime-ac-member-candidates "hea")))
         (ensime-assert (member "headOption" candidates)))

       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )

   (ensime-async-test
    "Test completing symbols."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "import java.io.File"

                                 "class HelloWorld{"

                                 "  def main {"
                                 "    val f = new Fi/*1*/"
                                 "  }"

                                 "  def blarg:Int = 5"

                                 "  def add(a:Int):Int = {"
                                 "    a + bl/*2*/"
                                 "  }"

                                 "}"
                                 )
                     ))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))
       ;; Set cursor to symbol in method body..
       (find-file (car src-files))

       ;; constructor completion
       (ensime-test-eat-mark "1")
       (let* ((candidates (ensime-ac-name-candidates "Fi")))
         (ensime-assert (member "File" candidates)))

       ;; local method name completion.
       (ensime-test-eat-mark "2")
       (let* ((candidates (ensime-ac-name-candidates "bl")))
         (ensime-assert (member "blarg" candidates)))

       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )


   (ensime-async-test
    "Test completing imports."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "hello_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "package com.helloworld"
                                 "import java.ut/*1*/"
                                 "class HelloWorld{"
                                 "import sc/*2*/"
                                 "}"
                                 )
                     ))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))
       (find-file (car src-files))

       ;; complete java package member
       (ensime-test-eat-mark "1")
       (let* ((candidates (ensime-ac-package-decl-candidates "ut")))
         (ensime-assert (member "util" candidates)))

       ;; complete scala package
       (ensime-test-eat-mark "2")
       (let* ((candidates (ensime-ac-package-decl-candidates "sc")))
         (ensime-assert (member "scala" candidates)))

       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )


   (ensime-async-test
    "Test formatting source."
    (let* ((proj (ensime-create-tmp-project
                  `((:name
                     "format_world.scala"
                     :contents ,(ensime-test-concat-lines
                                 "class HelloWorld{"
                                 "def foo:Int=1"
                                 "}"
                                 )
                     ))))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))
       ;; Set cursor to symbol in method body..
       (find-file (car src-files))
       (save-buffer)
       (ensime-format-source)))

    ((:return-value val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))
       ;; Set cursor to symbol in method body..
       (find-file (car src-files))
       (let ((src (buffer-substring-no-properties
                   (point-min) (point-max))))
         (ensime-assert-equal src (ensime-test-concat-lines
                                   "class HelloWorld {"
                                   "  def foo: Int = 1"
                                   "}"
                                   )))

       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       )))


   (ensime-async-test
    "Load and compile 'hello world'."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info)
     (ensime-assert (stringp (plist-get connection-info :version))))

    ((:full-typecheck-finished val)
     (let ((proj (ensime-test-var-get :proj)))
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )




   (ensime-async-test
    "Get package info for com.helloworld."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let ((proj (ensime-test-var-get :proj)))
       (let ((info (ensime-rpc-inspect-package-by-path
                    "com.helloworld")))
         (ensime-assert (not (null info)))
         (ensime-assert-equal (ensime-package-full-name info) "com.helloworld")
         ;; Should be one class, one object
         (ensime-assert-equal 2 (length (ensime-package-members info)))
         )
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )



   (ensime-async-test
    "Verify re-typecheck on save-buffer."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files))
            (notes (plist-get val :notes)))
       (ensime-assert-equal (length notes) 0)
       (find-file (car src-files))
       (goto-char (point-min))
       (insert "lksdjfldkjf ")

       ;; save-buffer should trigger a recheck...
       (save-buffer)
       ))

    ((:full-typecheck-finished val)
     (let ((proj (ensime-test-var-get :proj))
           (notes (plist-get val :notes)))
       (ensime-assert (> (length notes) 0))
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )


   (ensime-async-test
    "Test get symbol info at point."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:full-typecheck-finished val)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))
       ;; Set cursor to symbol in method body..
       (find-file (car src-files))
       (goto-char 163)
       (let* ((info (ensime-rpc-symbol-at-point))
              (pos (ensime-symbol-decl-pos info)))
         ;; New position should be at formal parameter...
         (ensime-assert-equal (ensime-pos-offset pos) 141)
         )
       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )


   (ensime-async-test
    "Test get repl config."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:compiler-ready status)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))

       (let ((conf (ensime-rpc-repl-config)))
         (ensime-assert (not (null conf)))
         (ensime-assert (not (null (plist-get conf :classpath)))))

       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )

   (ensime-async-test
    "Test get debug config."
    (let* ((proj (ensime-create-tmp-project
                  ensime-tmp-project-hello-world))
           (src-files (plist-get proj :src-files)))
      (ensime-test-var-put :proj proj)
      (find-file (car src-files))
      (ensime))

    ((:connected connection-info))

    ((:compiler-ready status)
     (let* ((proj (ensime-test-var-get :proj))
            (src-files (plist-get proj :src-files)))

       (let ((conf (ensime-rpc-debug-config)))
         (ensime-assert (not (null conf)))
         (ensime-assert (not (null (plist-get conf :classpath))))
         (ensime-assert (not (null (plist-get conf :sourcepath))))
         )

       (ensime-cleanup-tmp-project proj)
       (ensime-kill-all-ensime-servers)
       ))
    )

   ))

(defun ensime-run-tests ()
  "Run all regression tests for ensime-mode."
  (interactive)
  (ensime-run-fast-tests)
  (ensime-run-slow-tests))





(provide 'ensime-test)