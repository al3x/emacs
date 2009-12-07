;;; textile-minor-mode.el --- Textile markup editing minor mode

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Vadim Atlygin <vadim.atlygin@gmail.com>

;; Based on textile-mode created by Julien Barnier <julien@nozav.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;


;; Known bugs or limitations:

;; - if several {style}, [lang] or (class) attributes are given for
;;	 the same block, only the first one of each type will be
;;	 highlighted.
;;
;; - some complex imbrications of inline markup and attributes are
;;	 not well-rendered (for example, *strong *{something}notstrong*)
;;


;;; Code:



(defvar textile-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map [foo] 'textile-do-foo)
	map)
  "Keymap for `textile-mode'.")


(defun textile-re-concat (l)
  "Concatenate the elements of a list with a \\| separator and
non-matching parentheses"
  (concat
   "\\(?:"
   (mapconcat 'identity l "\\|")
   "\\)"))


(setq textile-attributes
	  '("{[^}]*}" "([^)]*)" "\\[[^]]*\\]"))

(setq textile-blocks
	  '("^h1" "^h2" "^h3" "^h4" "^h5" "^h6" "^p" "^bq" "^fn[0-9]+" "^#+ " "^\\*+ " "^table"))

(setq textile-inline-markup
	  '("\\*" "\\*\\*" "_" "__" "\\?\\?" "@" "-" "\\+" "^" "~" "%"))

(setq textile-alignments
	  '( "<>" "<" ">" "=" "(+" ")+"))

(setq textile-table-alignments
	  '( "<>" "<" ">" "=" "_" "\\^" "~" "\\\\[0-9]+" "/[0-9]+"))

; from gnus-button-url-regexp
(setq textile-url-regexp "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?[-a-z0-9_=!?#$@~%&*+\\/:;.,[:word:]]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)")


(defun textile-block-matcher (bloc)
  "Return the matcher regexp for a block element"
  (concat
   "^"
   bloc
   (textile-re-concat textile-alignments) "?"
   (textile-re-concat textile-attributes) "*"
   "\\. "
   "\\(\\(?:.\\|\n\\)*?\\)\n\n"))

(defun textile-attribute-matcher (attr-start attr-end)
  "Return the matcher regexp for an attribute"
  (concat
   (textile-re-concat (append textile-blocks textile-inline-markup))
   (textile-re-concat textile-alignments) "*"
   (textile-re-concat textile-attributes) "*"
   "\\(" attr-start "[^"
   (if (string-equal attr-end "\\]") "]" attr-end)
   "]*" attr-end "\\)"))

(defun textile-inline-markup-matcher (markup)
  "Return the matcher regexp for an inline markup"
  (concat
	"\\W\\("
   markup
   "\\(\\w\\|\\w.*?\\w\\|[[{(].*?\\w\\)"
   markup
	"\\)\\W"))

(defun textile-list-bullet-matcher (bullet)
  "Return the matcher regexp for a list bullet"
  (concat
   "^\\(" bullet "\\)"
   (textile-re-concat textile-alignments) "*"
   (textile-re-concat textile-attributes) "*"))

(defun textile-alignments-matcher ()
  "Return the matcher regexp for an alignments or indentation"
  (concat
   "\\(?:" (textile-re-concat textile-blocks) "\\|" "!" "\\)"
   "\\(" (textile-re-concat textile-alignments) "+" "\\)"))

(defun textile-table-matcher ()
  "Return the matcher regexp for a table row or header"
  (concat
   "\\(?:"
   "^table" (textile-re-concat textile-table-alignments) "*" (textile-re-concat textile-attributes) "*" "\\. *$"
   "\\|"
   "^" (textile-re-concat textile-table-alignments) "*" (textile-re-concat textile-attributes) "*" "\\(?:\\. *|\\)"
   "\\|"
   "|" (textile-re-concat textile-table-alignments) "*" (textile-re-concat textile-attributes) "*" "\\(?:\\. \\)?"
   "\\|"
   "| *$"
   "\\)"))

(defun textile-link-matcher ()
  "Return the matcher regexp for a link"
  (concat
   "\\(?:"
   "\\(?:"
   "\"\\(.*?\\)\":?"
   "\\|"
   "\\[\\(.*?\\)\\]"
   "\\)?"
   textile-url-regexp
	"\\|"
	"\".*?\":[^ \n\t]+"
   "\\)"))

(defun textile-image-matcher ()
  "Return the matcher regexp for an image link"
  (concat
   "!"
   (textile-re-concat textile-alignments) "*"
   "/?\\(\\w[^ \n\t]*?\\)\\(?: *(\\(.*?\\))\\|\\w\\)"
   "!:?"))

(defun textile-acronym-matcher ()
  "Return the matcher regexp for an acronym"
  (concat
   "\\w+" "(.*?)"))

(defvar textile-font-lock-keywords
	  (list
	   ;; headers
	   `(,(textile-block-matcher "h1") 1 'textile-h1-face t t)
	   `(,(textile-block-matcher "h2") 1 'textile-h2-face t t)
	   `(,(textile-block-matcher "h3") 1 'textile-h3-face t t)
	   `(,(textile-block-matcher "h4") 1 'textile-h4-face t t)
	   `(,(textile-block-matcher "h5") 1 'textile-h5-face t t)
	   `(,(textile-block-matcher "h6") 1 'textile-h6-face t t)
	   ;; blockquotes
	   `(,(textile-block-matcher "bq") 1 'textile-blockquote-face t t)
	   ;; footnotes
	   `(,(textile-block-matcher "fn[0-9]+") 1 'textile-footnote-face t t)
	   ;; footnote marks
	   '("\\w\\([[0-9]+]\\)" 1 'textile-footnotemark-face prepend t)
	   ;; acronyms
	   `(,(textile-acronym-matcher) 0 'textile-acronym-face t t)

	   ;; emphasis
	   `(,(textile-inline-markup-matcher "__") 1 'textile-emph-face prepend t)
	   `(,(textile-inline-markup-matcher "_") 1 'textile-emph-face prepend t)
	   '("<em>\\(.\\|\n\\)*?</em>" 0 'textile-emph-face prepend t)
	   ;; strength
	   `(,(textile-inline-markup-matcher "\\*\\*") 1 'textile-strong-face prepend t)
	   `(,(textile-inline-markup-matcher "\\*") 1 'textile-strong-face prepend t)
	   '("<strong>\\(.\\|\n\\)*?</strong>" 0 'textile-strong-face prepend t)
	   ;; citation
	   `(,(textile-inline-markup-matcher "\\?\\?") 1 'textile-citation-face prepend t)
	   ;; code
	   `(,(textile-inline-markup-matcher "@") 1 'textile-code-face prepend t)
	   ;; deletion
	   `(,(textile-inline-markup-matcher "-") 1 'textile-deleted-face prepend t)
	   ;; insertion
	   `(,(textile-inline-markup-matcher "\\+") 1 'textile-inserted-face prepend t)
	   ;; superscript
	   `(,(textile-inline-markup-matcher "\\^") 1 'textile-superscript-face prepend t)
	   ;; subscript
	   `(,(textile-inline-markup-matcher "~") 1 'textile-subscript-face prepend t)
	   ;; span
	   `(,(textile-inline-markup-matcher "%") 1 'textile-span-face prepend t)

	   ;; image link
	   `(,(textile-image-matcher) 0 'textile-image-face t t)

	   ;; ordered list bullet
	   `(,(textile-list-bullet-matcher "#+") 1 'textile-ol-bullet-face)
	   ;; unordered list bullet
	   `(,(textile-list-bullet-matcher "\\*+") 1 'textile-ul-bullet-face)

	   ;; style
	   `(,(textile-attribute-matcher "{" "}") 1 'textile-style-face t t)
	   ;; class
	   `(,(textile-attribute-matcher "(" ")") 1 'textile-class-face t t)
	   ;; lang
	   `(,(textile-attribute-matcher "\\[" "\\]") 1 'textile-lang-face t t)

	   ;; alignments and indentation
	   `(,(textile-alignments-matcher) 1 'textile-alignments-face t t)

	   ;; tables
	   `(,(textile-table-matcher) 0 'textile-table-face t t)

	   ;; links
	   `(,(textile-link-matcher) 0 'textile-link-face t t)

		;; <pre> blocks
	   '("<pre>\\(.\\|\n\\)*?</pre>" 0 'textile-pre-face t t)
	   ;; <code> blocks
	   '("<code>\\(.\\|\n\\)*?</code>" 0 'textile-code-face t t))
	  "Keywords/Regexp for fontlocking of textile-mode")


;; (defvar textile-imenu-generic-expression
;; ...)

;; (defvar textile-outline-regexp
;;	 ...)


(define-minor-mode textile-minor-mode
  "Textile minor mode"
  ;; the initial value
  nil
  ;; the indicator for the mode line
  " Textile"
  ;; the keymap
  nil
  ;; the body
  (if textile-minor-mode
	  (progn
		(make-local-variable 'textile-minor-mode-initial-font-lock-keywords)
		(setq textile-minor-mode-initial-font-lock-keywords font-lock-keywords)
		(font-lock-add-keywords nil textile-font-lock-keywords)
		(font-lock-fontify-buffer))
	(progn
	  (setq font-lock-keywords textile-minor-mode-initial-font-lock-keywords)
	  (font-lock-fontify-buffer))))
;; FACES

(defgroup textile-faces nil
  "Faces used by textile-mode for syntax highlighting"
  :group 'faces)

(defface textile-h1-face
  '((t (:weight bold)))
  "Face used to highlight h1 headers."
  :group 'textile-faces)

(defface textile-h2-face
  '((t (:weight bold)))
  "Face used to highlight h2 headers."
  :group 'textile-faces)

(defface textile-h3-face
  '((t (:weight bold)))
  "Face used to highlight h3 headers."
  :group 'textile-faces)

(defface textile-h4-face
  '((t (:weight bold)))
  "Face used to highlight h4 headers."
  :group 'textile-faces)

(defface textile-h5-face
  '((t (:weight bold)))
  "Face used to highlight h5 headers."
  :group 'textile-faces)

(defface textile-h6-face
  '((t (:weight bold)))
  "Face used to highlight h6 headers."
  :group 'textile-faces)

(defface textile-blockquote-face
  '((t (:foreground "ivory4")))
  "Face used to highlight bq blocks."
  :group 'textile-faces)

(defface textile-footnote-face
  '((t (:foreground "orange red")))
  "Face used to highlight footnote blocks."
  :group 'textile-faces)

(defface textile-footnotemark-face
  '((t (:foreground "orange red")))
  "Face used to highlight footnote marks."
  :group 'textile-faces)

(defface textile-style-face
  '((t (:foreground "sandy brown")))
  "Face used to highlight style parameters."
  :group 'textile-faces)

(defface textile-class-face
  '((t (:foreground "yellow green")))
  "Face used to highlight class and id parameters."
  :group 'textile-faces)

(defface textile-lang-face
  '((t (:foreground "sky blue")))
  "Face used to highlight lang parameters."
  :group 'textile-faces)

(defface textile-emph-face
  '((t (:slant italic)))
  "Face used to highlight emphasized words."
  :group 'textile-faces)

(defface textile-strong-face
  '((t (:weight bold)))
  "Face used to highlight strong words."
  :group 'textile-faces)

(defface textile-code-face
  '((t (:foreground "ivory3")))
  "Face used to highlight inline code."
  :group 'textile-faces)

(defface textile-citation-face
  '((t (:slant italic)))
  "Face used to highlight citations."
  :group 'textile-faces)

(defface textile-deleted-face
  '((t (:strike-through t)))
  "Face used to highlight deleted words."
  :group 'textile-faces)

(defface textile-inserted-face
  '((t (:underline t)))
  "Face used to highlight inserted words."
  :group 'textile-faces)

(defface textile-superscript-face
  '((t (:height 1.1)))
  "Face used to highlight superscript words."
  :group 'textile-faces)

(defface textile-subscript-face
  '((t (:height 0.8)))
  "Face used to highlight subscript words."
  :group 'textile-faces)

(defface textile-span-face
  '((t (:foreground "pink")))
  "Face used to highlight span words."
  :group 'textile-faces)

(defface textile-alignments-face
  '((t (:foreground "cyan")))
  "Face used to highlight alignments."
  :group 'textile-faces)

(defface textile-ol-bullet-face
  '((t (:foreground "red")))
  "Face used to highlight ordered lists bullets."
  :group 'textile-faces)

(defface textile-ul-bullet-face
  '((t (:foreground "blue")))
  "Face used to highlight unordered list bullets."
  :group 'textile-faces)

(defface textile-pre-face
  '((t (:foreground "green")))
  "Face used to highlight <pre> blocks."
  :group 'textile-faces)

(defface textile-code-face
  '((t (:foreground "yellow")))
  "Face used to highlight <code> blocks."
  :group 'textile-faces)

(defface textile-table-face
  '((t (:foreground "red")))
  "Face used to highlight tables."
  :group 'textile-faces)

(defface textile-link-face
  '((t (:foreground "DeepSkyBlue")))
  "Face used to highlight links."
  :group 'textile-faces)

(defface textile-image-face
  '((t (:foreground "pink")))
  "Face used to highlight image links."
  :group 'textile-faces)

(defface textile-acronym-face
  '((t (:foreground "cyan")))
  "Face used to highlight acronyms links."
  :group 'textile-faces)


(setq textile-ignore-block-start "<[Cc][Oo][Dd][Ee]>\\|<[Pp][Rr][Ee]>")
(setq textile-ignore-block-end "</[Cc][Oo][Dd][Ee]>\\|</[Pp][Rr][Ee]>")

(defun textile-replace (tobufferstart regexp tostring subexpr)
  "Replaces all the occurrences of regexp in the buffer"
  (funcall tobufferstart)
  (while (re-search-forward (concat regexp "\\|" textile-ignore-block-start) nil t)
	(if (string-match textile-ignore-block-start (match-string 0))
		(re-search-forward textile-ignore-block-end nil t)
	  (replace-match tostring nil nil nil subexpr)))
)

(defun textile-to-html-internal ( tobufferstart )
  "Converts current buffer to HTML
   Uses tobufferstart to go to buffer start :)"
  (save-excursion
	(textile-replace tobufferstart (textile-inline-markup-matcher "_") "<em>\\2</em>" 1)
	(textile-replace tobufferstart (textile-inline-markup-matcher "__") "<em>\\2</em>" 1)
	(textile-replace tobufferstart (textile-inline-markup-matcher "\\*") "<strong>\\2</strong>" 1)
	(textile-replace tobufferstart (textile-inline-markup-matcher "\\*\\*") "<strong>\\2</strong>" 1)
	(textile-replace tobufferstart (textile-link-matcher) "<a href='\\3'>\\1\\2</a>" nil)
	(textile-replace tobufferstart (textile-image-matcher) "<img src='\\1' alt='\\2' title='\\2'/>" nil)
	(textile-replace tobufferstart "\n[^#].*\n\\(\\)#" "<ol>\n" 1)
	(textile-replace tobufferstart "\n#.*\n\\(\\)\n" "</ol>\n" 1)
	(textile-replace tobufferstart "\n[^*].*\n\\(\\)\\*" "<ul>\n" 1)
	(textile-replace tobufferstart "\n\\*.*\n\\(\\)[\n\']" "</ul>\n" 1)
	(textile-replace tobufferstart "^[#*] \\(.*\\)$" "<li>\\1</li>" nil)
	(textile-replace tobufferstart "\n\\(\\(?:.\\|\n[^\n]\\)*\\)\n" "\n<p>\\1</p>\n" nil)
	)
)

(defun textile-to-html-buffer ()
  "Converts current buffer to HTML"
  (interactive)
  (textile-to-html-internal 'beginning-of-buffer)
  )

(defun textile-to-html-buffer-respect-weblogger ()
  "Converts current buffer to HTML with respect to weblogger
   It just means that text before --text follows this line-- would be left alone."
  (interactive)
  (textile-to-html-internal (lambda () (progn (beginning-of-buffer) (search-forward "--text follows this line--") (forward-line) )) )
)

(provide 'textile-minor-mode)
 ;;; textile-mode.el ends here