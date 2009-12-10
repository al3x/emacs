; TextMate
(require 'textmate)
(textmate-mode t)

; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)

; Ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

; Nav
(require 'nav)

; Scala
(require 'scala-mode-auto)

; Textile
(require 'textile-minor-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-minor-mode))

; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

; JavaScript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; Ruby
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(setq auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist))

; Rails
(require 'rinari)
(setq rinari-tags-file-name "TAGS")

; nxhtml (HTML/ERB template support)
(load "~/.emacs.d/vendor/nxhtml/autostart.el")
(setq
  nxhtml-global-minor-mode t
  mumamo-chunk-coloring 'submode-colored
  nxhtml-skip-welcome t
  indent-region-mode t
  rng-nxml-auto-validate-flag nil
  nxml-degraded t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))

(eval-after-load 'nxhtml
  '(eval-after-load 'color-theme
     (custom-set-faces
       '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) (:background "#242424"))))
       '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) (:background "#373736"))))
)))

; Org
(require 'org-install)
(setq org-mobile-directory "~/org/")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

; swank-clojure
(setq swank-clojure-jar-path "~/src/third_party/clojure/clojure/clojure-1.1.0-master-SNAPSHOT.jar")
(setq swank-clojure-extra-classpaths
      (list "~/src/third_party/clojure/clojure-contrib/clojure-contrib.jar"))

(require 'clojure-mode)
(require 'swank-clojure-autoload)
(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)
