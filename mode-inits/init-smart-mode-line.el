(setq sml/theme 'respectful)
(sml/setup)

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)

(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " vl")
(add-to-list 'sml/hidden-modes " Wrap")
(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " FlyC")
(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " oracle")

(add-to-list 'sml/replacer-regexp-list '("^~/src/dobt/" ":DOBT:"))

(custom-set-variables
 '(custom-safe-themes (quote ("9527feeeec43970b1d725bdc04e97eb2b03b15be982ac50089ad223d3c6f2920" default))))
(custom-set-faces)

(defadvice sml/setup (after load-theme-again activate)
  "Load the damn theme again."
  (load-theme my-cur-theme t))

(ad-activate 'sml/setup)
