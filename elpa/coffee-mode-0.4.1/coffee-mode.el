;;; coffee-mode.el --- Major mode for CoffeeScript files

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Version: 0.4.1
;; Keywords: CoffeeScript major mode
;; Author: Chris Wanstrath <chris@ozmm.org>
;; URL: http://github.com/defunkt/coffee-mode

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; CoffeeScript mode is an Emacs major mode for [CoffeeScript][cs],
;; unfancy JavaScript.  It provides syntax highlighting, indentation
;; support, imenu support, a menu bar, and a few cute commands.

;; Installing this package enables CoffeeScript mode for file named
;; *.coffee and Cakefile.

;; Commands:

;; M-x coffee-compile-file compiles the current file as a JavaScript
;; file.  Operating on "basic.coffee" and running this command will
;; save a "basic.js" in the same directory.  Subsequent runs will
;; overwrite the file.
;;
;; If there are compilation errors and we the compiler have returned a
;; line number to us for the first error, the point is moved to that
;; line, so you can investigate.  If this annoys you, you can set
;; `coffee-compile-jump-to-error` to `nil`.
;;
;; M-x coffee-compile-buffer compiles the current buffer to JavaScript
;; using the command specified by the `coffee-command` variable, and
;; opens the contents in a new buffer using the mode configured for
;; ".js" files.
;;
;; M-x coffee-compile-region compiles the selected region to
;; JavaScript using the same configuration variables as
;; `coffee-compile-buffer`.
;;
;; `C-c C-o C-s' (coffee-cos-mode) toggles a minor mode implementing
;; "compile-on-save" behavior.
;;
;; M-x coffee-repl starts a repl via `coffee-command` in a new buffer.

;; Options:
;;
;; `coffee-tab-width' - Tab width to use when indenting.
;; `coffee-command'   - CoffeeScript command for evaluating code.
;;                      Must be in your path.
;; `coffee-args-repl' - Command line arguments for `coffee-command'
;;                      when starting a REPL.
;; `coffee-args-compile'          - Arguments for `coffee-command'
;;                                  when compiling a file.
;; `coffee-compiled-buffer-name'  - Name of the scratch buffer used
;;                                  when compiling CoffeeScript.
;; `coffee-compile-jump-to-error' - Whether to jump to the first error
;;                                  if compilation fails.

;; Please file bugs at <http://github.com/defunkt/coffee-mode/issues>

;;; Code:

(require 'comint)
(require 'easymenu)
(require 'font-lock)

(eval-when-compile
  (require 'cl))

;;
;; Customizable Variables
;;

(defconst coffee-mode-version "0.4.1"
  "The version of `coffee-mode'.")

(defgroup coffee nil
  "A CoffeeScript major mode."
  :group 'languages)

(defcustom coffee-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'coffee)

(defcustom coffee-command "coffee"
  "The CoffeeScript command used for evaluating code."
  :type 'string
  :group 'coffee)

(defcustom js2coffee-command "js2coffee"
  "The js2coffee command used for evaluating code."
  :type 'string
  :group 'coffee)

(defcustom coffee-args-repl '("-i")
  "The arguments to pass to `coffee-command' to start a REPL."
  :type 'list
  :group 'coffee)

(defcustom coffee-args-compile '("-c")
  "The arguments to pass to `coffee-command' to compile a file."
  :type 'list
  :group 'coffee)

(defcustom coffee-compiled-buffer-name "*coffee-compiled*"
  "The name of the scratch buffer used for compiled CoffeeScript."
  :type 'string
  :group 'coffee)

(defcustom coffee-compile-jump-to-error t
  "Whether to jump to the first error if compilation fails.
Please note that the coffee compiler doesn't always give a line
number for the issue and in that case it is not possible to jump
to the error."
  :type 'boolean
  :group 'coffee)

(defcustom coffee-watch-buffer-name "*coffee-watch*"
  "The name of the scratch buffer used when using the --watch flag
with CoffeeScript."
  :type 'string
  :group 'coffee)

(defcustom coffee-mode-hook nil
  "Hook called by `coffee-mode'."
  :type 'hook
  :group 'coffee)

(defvar coffee-mode-map (make-keymap)
  "Keymap for CoffeeScript major mode.")

;;
;; Commands
;;

(defun coffee-repl ()
  "Launch a CoffeeScript REPL using `coffee-command' as an inferior mode."
  (interactive)

  (unless (comint-check-proc "*CoffeeREPL*")
    (set-buffer
     (apply 'make-comint "CoffeeREPL"
            coffee-command nil coffee-args-repl)))

  (pop-to-buffer "*CoffeeREPL*"))

(defun coffee-compiled-file-name (&optional filename)
  "Returns the name of the JavaScript file compiled from a CoffeeScript file.
If FILENAME is omitted, the current buffer's file name is used."
  (concat (file-name-sans-extension (or filename (buffer-file-name))) ".js"))

(defun coffee-compile-file ()
  "Compiles and saves the current file to disk."
  (interactive)
  (let ((compiler-output (shell-command-to-string (coffee-command-compile (buffer-file-name)))))
    (if (string= compiler-output "")
        (message "Compiled and saved %s" (coffee-compiled-file-name))
      (let* ((msg (car (split-string compiler-output "[\n\r]+")))
             (line (and (string-match "on line \\([0-9]+\\)" msg)
                        (string-to-number (match-string 1 msg)))))
        (message msg)
        (when (and coffee-compile-jump-to-error line (> line 0))
          (goto-char (point-min))
          (forward-line (1- line)))))))

(defun coffee-compile-buffer ()
  "Compiles the current buffer and displays the JavaScript in a buffer
called `coffee-compiled-buffer-name'."
  (interactive)
  (save-excursion
    (coffee-compile-region (point-min) (point-max))))

(defun coffee-compile-region (start end)
  "Compiles a region and displays the JavaScript in a buffer called
`coffee-compiled-buffer-name'."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (apply (apply-partially 'call-process-region start end coffee-command nil
                          (get-buffer-create coffee-compiled-buffer-name)
                          nil)
         (append coffee-args-compile (list "-s" "-p")))
  (switch-to-buffer (get-buffer coffee-compiled-buffer-name))
  (let ((buffer-file-name "tmp.js")) (set-auto-mode))
  (goto-char (point-min)))

(defun coffee-js2coffee-replace-region (start end)
  "Convert JavaScript in the region into CoffeeScript."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end
                       js2coffee-command nil
                       (current-buffer))
  (delete-region start end))

(defun coffee-version ()
  "Show the `coffee-mode' version in the echo area."
  (interactive)
  (message (concat "coffee-mode version " coffee-mode-version)))

(defun coffee-watch (dir-or-file)
  "Run `coffee-run-cmd' with the --watch flag on a directory or file."
  (interactive "fDirectory or File: ")
  (let ((coffee-compiled-buffer-name coffee-watch-buffer-name)
        (args (mapconcat 'identity (append coffee-args-compile (list "--watch" (expand-file-name dir-or-file))) " ")))
    (coffee-run-cmd args)))

;;
;; Menubar
;;

(easy-menu-define coffee-mode-menu coffee-mode-map
  "Menu for CoffeeScript mode"
  '("CoffeeScript"
    ["Compile File" coffee-compile-file]
    ["Compile Buffer" coffee-compile-buffer]
    ["Compile Region" coffee-compile-region]
    ["REPL" coffee-repl]
    "---"
    ["Version" coffee-show-version]
    ))

;;
;; Define Language Syntax
;;

;; String literals
(defvar coffee-string-regexp "\"\\([^\\]\\|\\\\.\\)*?\"\\|'\\([^\\]\\|\\\\.\\)*?'")

;; Instance variables (implicit this)
(defvar coffee-this-regexp "@\\(\\w\\|_\\)*\\|this")

;; Prototype::access
(defvar coffee-prototype-regexp "\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\)::\\(\\(\\w\\|\\.\\|_\\| \\|$\\)+?\\):")

;; Assignment
(defvar coffee-assign-regexp "\\(\\(\\w\\|\\.\\|_\\|$\\)+?\s*\\):")

;; Lambda
(defvar coffee-lambda-regexp "\\((.+)\\)?\\s *\\(->\\|=>\\)")

;; Namespaces
(defvar coffee-namespace-regexp "\\b\\(class\\s +\\(\\S +\\)\\)\\b")

;; Booleans
(defvar coffee-boolean-regexp "\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\|null\\|undefined\\)\\b")

;; Regular Expressions
(defvar coffee-regexp-regexp "\\/\\(\\\\.\\|\\[\\(\\\\.\\|.\\)+?\\]\\|[^/]\\)+?\\/")

;; JavaScript Keywords
(defvar coffee-js-keywords
      '("if" "else" "new" "return" "try" "catch"
        "finally" "throw" "break" "continue" "for" "in" "while"
        "delete" "instanceof" "typeof" "switch" "super" "extends"
        "class" "until" "loop"))

;; Reserved keywords either by JS or CS.
(defvar coffee-js-reserved
      '("case" "default" "do" "function" "var" "void" "with"
        "const" "let" "debugger" "enum" "export" "import" "native"
        "__extends" "__hasProp"))

;; CoffeeScript keywords.
(defvar coffee-cs-keywords
      '("then" "unless" "and" "or" "is"
        "isnt" "not" "of" "by" "where" "when"))

;; Regular expression combining the above three lists.
(defvar coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))


;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar coffee-font-lock-keywords
  ;; *Note*: order below matters. `coffee-keywords-regexp' goes last
  ;; because otherwise the keyword "state" in the function
  ;; "state_entry" would be highlighted.
  `((,coffee-string-regexp . font-lock-string-face)
    (,coffee-this-regexp . font-lock-variable-name-face)
    (,coffee-prototype-regexp . font-lock-variable-name-face)
    (,coffee-assign-regexp . font-lock-type-face)
    (,coffee-regexp-regexp . font-lock-constant-face)
    (,coffee-boolean-regexp . font-lock-constant-face)
    (,coffee-keywords-regexp . font-lock-keyword-face)))

;;
;; Helper Functions
;;

(defun coffee-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For details, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defun coffee-command-compile (file-name)
  "Run `coffee-command' to compile FILE."
  (let ((full-file-name (expand-file-name file-name)))
    (mapconcat 'identity (append (list coffee-command) coffee-args-compile (list full-file-name)) " ")))

(defun coffee-run-cmd (args)
  "Run `coffee-command' with the given arguments, and display the
output in a compilation buffer."
  (interactive "sArguments: ")
  (let ((compilation-buffer-name-function (lambda (this-mode)
                                            (generate-new-buffer-name coffee-compiled-buffer-name))))
    (compile (concat coffee-command " " args))))

;;
;; imenu support
;;

;; This is a pretty naive but workable way of doing it. First we look
;; for any lines that starting with `coffee-assign-regexp' that include
;; `coffee-lambda-regexp' then add those tokens to the list.
;;
;; Should cover cases like these:
;;
;; minus: (x, y) -> x - y
;; String::length: -> 10
;; block: ->
;;   print('potion')
;;
;; Next we look for any line that starts with `class' or
;; `coffee-assign-regexp' followed by `{` and drop into a
;; namespace. This means we search one indentation level deeper for
;; more assignments and add them to the alist prefixed with the
;; namespace name.
;;
;; Should cover cases like these:
;;
;; class Person
;;   print: ->
;;     print 'My name is ' + this.name + '.'
;;
;; class Policeman extends Person
;;   constructor: (rank) ->
;;     @rank: rank
;;   print: ->
;;     print 'My name is ' + this.name + " and I'm a " + this.rank + '.'
;;
;; TODO:
;; app = {
;;   window:  {width: 200, height: 200}
;;   para:    -> 'Welcome.'
;;   button:  -> 'OK'
;; }

(defun coffee-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (interactive)

  ;; This function is called within a `save-excursion' so we're safe.
  (goto-char (point-min))

  (let ((index-alist '()) assign pos indent ns-name ns-indent)
    ;; Go through every assignment that includes -> or => on the same
    ;; line or starts with `class'.
    (while (re-search-forward
            (concat "^\\(\\s *\\)"
                    "\\("
                      coffee-assign-regexp
                      ".+?"
                      coffee-lambda-regexp
                    "\\|"
                      coffee-namespace-regexp
                    "\\)")
            (point-max)
            t)

      ;; If this is the start of a new namespace, save the namespace's
      ;; indentation level and name.
      (when (match-string 8)
        ;; Set the name.
        (setq ns-name (match-string 8))

        ;; If this is a class declaration, add :: to the namespace.
        (setq ns-name (concat ns-name "::"))

        ;; Save the indentation level.
        (setq ns-indent (length (match-string 1))))

      ;; If this is an assignment, save the token being
      ;; assigned. `Please.print:` will be `Please.print`, `block:`
      ;; will be `block`, etc.
      (when (setq assign (match-string 3))
          ;; The position of the match in the buffer.
          (setq pos (match-beginning 3))

          ;; The indent level of this match
          (setq indent (length (match-string 1)))

          ;; If we're within the context of a namespace, add that to the
          ;; front of the assign, e.g.
          ;; constructor: => Policeman::constructor
          (when (and ns-name (> indent ns-indent))
            (setq assign (concat ns-name assign)))

          ;; Clear the namespace if we're no longer indented deeper
          ;; than it.
          (when (and ns-name (<= indent ns-indent))
            (setq ns-name nil)
            (setq ns-indent nil))

          ;; Add this to the alist. Done.
          (push (cons assign pos) index-alist)))

    ;; Return the alist.
    index-alist))

;;
;; Indentation
;;

;;; The theory is explained in the README.

(defun coffee-indent-line ()
  "Indent current line as CoffeeScript."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent (coffee-previous-indent))
            (cur-indent (current-indentation)))
        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char coffee-tab-width))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) coffee-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun coffee-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (and (looking-at "^[ \t]*$") (not (bobp))) (forward-line -1))
        (current-indentation)))))

(defun coffee-newline-and-indent ()
  "Insert a newline and indent it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)) (indent-next nil))
    (delete-horizontal-space t)
    (newline)
    (insert-tab (/ prev-indent coffee-tab-width))

    ;; We need to insert an additional tab because the last line was special.
    (when (coffee-line-wants-indent)
      (insert-tab)))

  ;; Last line was a comment so this one should probably be,
  ;; too. Makes it easy to write multi-line comments (like the one I'm
  ;; writing right now).
  (when (coffee-previous-line-is-comment)
    (insert "# ")))

;; Indenters help determine whether the current line should be
;; indented further based on the content of the previous line. If a
;; line starts with `class', for instance, you're probably going to
;; want to indent the next line.

(defvar coffee-indenters-bol '("class" "for" "if" "try")
  "Keywords or syntax whose presence at the start of a line means the
next line should probably be indented.")

(defun coffee-indenters-bol-regexp ()
  "Builds a regexp out of `coffee-indenters-bol' words."
  (regexp-opt coffee-indenters-bol 'words))

(defvar coffee-indenters-eol '(?> ?{ ?\[)
  "Single characters at the end of a line that mean the next line
should probably be indented.")

(defun coffee-line-wants-indent ()
  "Return t if the current line should be indented relative to the
previous line."
  (interactive)

  (save-excursion
    (let ((indenter-at-bol) (indenter-at-eol))
      ;; Go back a line and to the first character.
      (forward-line -1)
      (backward-to-indentation 0)

      ;; If the next few characters match one of our magic indenter
      ;; keywords, we want to indent the line we were on originally.
      (when (looking-at (coffee-indenters-bol-regexp))
        (setq indenter-at-bol t))

      ;; If that didn't match, go to the back of the line and check to
      ;; see if the last character matches one of our indenter
      ;; characters.
      (when (not indenter-at-bol)
        (end-of-line)

        ;; Optimized for speed - checks only the last character.
        (let ((indenters coffee-indenters-eol))
          (while indenters
            (if (/= (char-before) (car indenters))
                (setq indenters (cdr indenters))
              (setq indenter-at-eol t)
              (setq indenters nil)))))

      ;; If we found an indenter, return `t'.
      (or indenter-at-bol indenter-at-eol))))

(defun coffee-previous-line-is-comment ()
  "Return t if the previous line is a CoffeeScript comment."
  (save-excursion
    (forward-line -1)
    (coffee-line-is-comment)))

(defun coffee-line-is-comment ()
  "Return t if the current line is a CoffeeScript comment."
  (save-excursion
    (backward-to-indentation 0)
    (= (char-after) (string-to-char "#"))))

;;
;; Define Major Mode
;;

;;;###autoload
(define-derived-mode coffee-mode fundamental-mode
  "Coffee"
  "Major mode for editing CoffeeScript."

  ;; key bindings
  (define-key coffee-mode-map (kbd "A-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "A-R") 'coffee-compile-region)
  (define-key coffee-mode-map (kbd "A-M-r") 'coffee-repl)
  (define-key coffee-mode-map [remap comment-dwim] 'coffee-comment-dwim)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent)
  (define-key coffee-mode-map "\C-c\C-o\C-s" 'coffee-cos-mode)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'coffee-indent-line)
  (set (make-local-variable 'tab-width) coffee-tab-width)

  ;; imenu
  (make-local-variable 'imenu-create-index-function)
  (setq imenu-create-index-function 'coffee-imenu-create-index)

  ;; no tabs
  (setq indent-tabs-mode nil))

;;
;; Compile-on-Save minor mode
;;

(defvar coffee-cos-mode-line " CoS")
(make-variable-buffer-local 'coffee-cos-mode-line)

(define-minor-mode coffee-cos-mode
  "Toggle compile-on-save for coffee-mode."
  :group 'coffee-cos :lighter coffee-cos-mode-line
  (cond
   (coffee-cos-mode
    (add-hook 'after-save-hook 'coffee-compile-file nil t))
   (t
    (remove-hook 'after-save-hook 'coffee-compile-file t))))

(provide 'coffee-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
;;; coffee-mode.el ends here
