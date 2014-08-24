(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(setq
  ruby-deep-indent-paren nil
  ruby-deep-arglist nil)
