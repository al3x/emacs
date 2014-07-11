; install El-Get if we don't already have it
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-recipes")

(setq
 el-get-byte-compile t
 el-get-git-shallow-clone t
 el-get-user-package-directory "~/.emacs.d/el-get-init"
 my-el-get-packages (append
                     '(
                       ag
                       apache-mode
                       cider
                       csv-mode
                       coffee-mode
                       company-mode
                       dash-at-point
                       dired+
                       dired-details+
                       dired-single
                       find-file-in-project
                       flx
                       flycheck
                       gist
                       go-company
                       go-eldoc
                       go-imports
                       go-mode
                       go-oracle
                       go-projectile
                       go-test
                       haskell-mode
                       inf-ruby
                       json-mode
                       json-reformat
                       json-snatcher
                       magit
                       mo-git-blame
                       markdown-mode
                       nginx-mode
                       org-mode
                       pbcopy
                       projectile
                       request
                       robe-mode
                       rubocop-mode
                       ruby-mode
                       scala-mode2
                       smart-mode-line
                       smartparens
                       smex
                       solarized-emacs
                       textile-mode
                       yaml-mode
                       )))

(el-get 'sync my-el-get-packages)
