;; lets you M-x without meta
;; yes, this overwrites the usual way of exiting Emacs
(global-set-key (kbd "C-x C-c") 'smex)
(global-set-key (kbd "C-x c") 'smex)
(global-set-key (kbd "C-x m") 'smex-major-mode-commands)
