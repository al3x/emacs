(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'haskell-mode "ghc" nil t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
