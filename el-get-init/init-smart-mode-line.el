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

(add-to-list 'sml/replacer-regexp-list '("^~/src/dobt/" ":DOBT:"))

(defadvice sml/setup (after load-theme-again activate)
  "Load the damn theme again."
  (load-theme my-cur-theme))

(ad-activate 'sml/setup)
