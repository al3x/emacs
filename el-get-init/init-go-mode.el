(add-hook 'go-mode-hook
          #'(lambda () (setq indent-tabs-mode nil)))

(when (load "flymake" t)
  (defun flymake-go-init ()
     (list "go" (list "build")))
   (add-to-list 'flymake-allowed-file-name-masks '("\\.go\\'" flymake-go-init)))
