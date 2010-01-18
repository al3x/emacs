;;; -*-Emacs-Lisp-*-
;;; scala-mode-indent.el - 

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id$

;;; License

;; SCALA LICENSE
;;  
;; Copyright (c) 2002-2010 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;  
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;  
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;  
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;  
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;  
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;  
;;  
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'scala-mode-indent)

(defcustom scala-mode-indent:step 2
  "Indentation step."
  :type 'integer
  :group 'scala)

(defun scala-parse-partial-sexp ()
  (parse-partial-sexp (point-min) (point)))

(defun scala-in-comment-p ()
  "Return t iff the point is inside a comment."
  ;; The two branches of the "if" below do not have the same behaviour
  ;; when the point is on the comment beginning/ending character(s).
  (or (scala-in-multi-line-comment-p)
      (scala-in-single-line-comment-p)))

(defun scala-in-single-line-comment-p ()
  "Return t iff the point is inside a single line comment."
  (let
      (begin
       end
       subst
       match)
    (save-excursion
      (setq end (point))
      (beginning-of-line)
      (setq begin (point))
      (setq subst (buffer-substring begin end))
      (setq match (string-match "//" subst))
      (if match t nil))))

(defun scala-in-multi-line-comment-p ()
  "Return t iff the point is inside a multi line comment."
  (if font-lock-mode
      (and (not (scala-in-single-line-comment-p))
	   (eq (get-text-property (point) 'face) 'font-lock-comment-face))
    nil))

(defun scala-in-string-p ()
  "Return t iff the point is inside a string."
  (if font-lock-mode
      (eq (get-text-property (point) 'face) 'font-lock-string-face)
    (let ((limit (point)))
      (beginning-of-line)
      (loop while (search-forward-regexp "\\(^\\|[^\\\\]\\)\"" limit 'move)
            count (not (scala-in-comment-p)) into quotes
            finally return (oddp quotes)))))

(defun scala-indentation ()
  "Return the suggested indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (or (and (scala-in-comment-p)
             (not (= (char-after) ?\/))
             (scala-comment-indentation))
        (scala-indentation-from-following)
	(scala-indentation-from-preceding)
	(scala-indentation-from-block)
        0)))

(defun scala-comment-indentation ()
  ;; Return suggested indentation inside of a comment.
  (forward-line -1)
  (beginning-of-line)
  (skip-syntax-forward " ")
  (if (looking-at "/\\*")
      (+ 1 (current-column))
    (current-column)))

(defun crawl-back-to-template ()
  (if (or (looking-at "extends")
	  (eq (char-before) ?\)))
      (backward-sexp)
    (backward-word))
  (cond 
   ((looking-at template-dcf-keywords-re) t)
   ((looking-at template-middle-keywords-re)
    (crawl-back-to-template))
   ((not (looking-at scala-keywords-re))
    (crawl-back-to-template))
   (t nil)))

;; assumes one point to right of nearest opening block
;; Oops we can case inside a block, no match required.
;; and no keyword is required.  Witness Actor's receive method.
;; Instead of looking for match|catch|receive|... which could be now anything
;; See if the FIRST word within the opening block is a case.
(defun scala-mode-match-catch-block-p ()
  (save-excursion
    (backward-word)
    (looking-at "match\\|catch\\|receive\\|react")))

;; assumes one point to right of nearest opening block 
(defun scala-block-indentation (&optional case-or-eob)
  (let ((cpos (point))
	(block-start-eol (scala-point-after (end-of-line)))
        (block-after-spc (scala-point-after (scala-forward-spaces))))
    (if (> block-after-spc block-start-eol)  ;; simple block open {
	(if (scala-mode-match-catch-block-p)
	    (progn
	      (if case-or-eob
		  (+ (current-indentation) scala-mode-indent:step)
		(+ (current-indentation) (* 2 scala-mode-indent:step))))
	  (progn                             ;; properly indent first line after template 
	    (backward-char)                  ;; on the '{'
	    (scala-backward-spaces)          
	    (unless (crawl-back-to-template)         ;; class,trait,object crawl to template start
		(goto-char cpos))
	    (+ (current-indentation) scala-mode-indent:step)))
      (progn                                 ;; block open with stuff { ...
	(goto-char cpos)
	(if (search-forward "=>" block-start-eol t)
	    (+ (current-indentation) scala-mode-indent:step)
	  (progn                            ;; properly indent mulitline args in a template
	    (scala-forward-spaces)
	    (current-column)))))))

(defun scala-indentation-from-following ()
  ;; Return suggested indentation based on the following part of the
  ;; current expression. Return nil if indentation cannot be guessed.  
  (save-excursion
    (scala-forward-spaces (scala-point-after (end-of-line)))
    (cond
     ((eobp) nil)
     ((= (char-syntax (char-after)) ?\))
      (let ((parse-sexp-ignore-comments t))
        (goto-char (1+ (scan-sexps (1+ (point)) -1)))
	(- (scala-block-indentation t) scala-mode-indent:step)))
     ((looking-at scala-expr-middle-re)
      ;; [...] this is a somewhat of a hack.
      (let ((matching-kw (cdr (assoc (match-string-no-properties 0)
                                     scala-expr-starter))))
        (while (and (search-backward-regexp matching-kw nil t)
                    (or (scala-in-comment-p) (scala-in-string-p)))))
      (scala-move-if (backward-word 1)
                     (looking-at scala-compound-expr-re))
      (current-column)))))

(defun scala-indentation-from-preceding ()
  ;; Return suggested indentation based on the preceding part of the
  ;; current expression. Return nil if indentation cannot be guessed.
  (save-excursion
    (let ((am-case (scala-indenting-case-line-p)))
      (scala-backward-spaces)
      (when (not (bobp))
	(cond
	 ((eq (char-syntax (char-before)) ?\()
	  (scala-block-indentation am-case))
	 ((eq (char-before) ?\=)
	  (backward-sexp)
	  (+ (current-indentation) scala-mode-indent:step))
	 ((progn 
	    (when (eq (char-before) ?\))
	      (backward-sexp)
	      (scala-backward-spaces))
	    (scala-looking-at-backward scala-expr-start-re))
	  (+ (current-indentation) scala-mode-indent:step))
	 (t nil))))))

(defun scala-indenting-case-line-p ()
  (save-excursion
    (beginning-of-line)
    (scala-forward-spaces)
    (looking-at "case")))

(defun scala-indentation-from-block ()
  ;; Return suggested indentation based on the current block.
  (save-excursion
    (let* ((am-case (scala-indenting-case-line-p))
	   (state (scala-parse-partial-sexp))
           (block-start (nth 1 state)))
      (if (not block-start)
          0
	(progn
	  (goto-char (1+ block-start))
	  (scala-block-indentation am-case))))))

(defun scala-indent-line-to (column)
  "Indent current line to COLUMN and perhaps move point.
The point is moved iff it is currently in the indentation, in which
case it is brought to the end of that indentation. Otherwise it does
not move."
  (if (<= (current-column) (current-indentation))
      (indent-line-to column)
    (save-excursion (indent-line-to column))))

(defun scala-indent-line ()
  "Indent current line as smartly as possible.
When called repeatedly, indent each time one stop further on the right."
  (interactive)
  (if (or (eq last-command this-command)
          (eq last-command 'scala-undent-line))
      (scala-indent-line-to (+ (current-indentation) scala-mode-indent:step))
    (let ((indentation (scala-indentation)))
      (scala-indent-line-to indentation))))

(defun scala-undent-line ()
  "Indent line to previous tab stop."
  (interactive)
  (scala-indent-line-to (max 0 (- (current-indentation) scala-mode-indent:step))))

(defun scala-electric-brace ()
  "Insert a brace, and if alone on a non-comment line, reindent."
  (interactive)
  (let ((on-empty-line-p (save-excursion
                           (beginning-of-line)
                           (looking-at "^\\s *$"))))
    ;; Calling self-insert-command will blink to the matching open-brace
    ;; (if blink-matching-paren is enabled); we first indent, then
    ;; call self-insert-command, so that the close-brace is correctly
    ;; positioned during the blink.
    (when on-empty-line-p
      (insert "}")
      (scala-indent-line)
      (delete-backward-char 1))
    (call-interactively 'self-insert-command)))


(defun scala-newline ()
  (interactive)
  (if (scala-in-multi-line-comment-p)
      (progn 
	(newline-and-indent)
	(insert "* "))
    (newline)))
