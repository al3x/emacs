(autoload 'go-mode "go-mode" nil t)

(add-hook 'go-mode-hook
          #'(lambda () (setq indent-tabs-mode nil)))

(when (load "flymake" t)
  (defun flymake-go-init ()
     (list "go" (list "build")))
   (add-to-list 'flymake-allowed-file-name-masks '("\\.go\\'" flymake-go-init)))

(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))

(add-hook 'go-mode-hook (lambda ()
                          (add-hook 'before-save-hook 'gofmt-before-save)))

(local-set-key (kbd "M-.") 'godef-jump)
