(autoload 'clojure-mode "clojure-mode" nil t)

(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
