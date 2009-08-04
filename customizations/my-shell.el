(setq eshell-cmpl-cycle-completions nil
  eshell-save-history-on-exit t
  eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(require 'ansi-color) (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)
;;(shell)
