(setq sml/theme 'respectful)

(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)

(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " vl")
(add-to-list 'sml/hidden-modes " Wrap")

(add-to-list 'sml/replacer-regexp-list '("^~/src/dobt/" ":DOBT:"))
