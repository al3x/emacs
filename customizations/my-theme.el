

(require 'color-theme)
(require 'color-theme-solarized)

(eval-after-load 'color-theme
  (progn (color-theme-initialize)))

(setq my-color-themes (list 'color-theme-solarized-dark 'color-theme-solarized-light))

(defun my-theme-set-default ()
  (interactive)
  (setq theme-current my-color-themes)
  (funcall (car theme-current)))

(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))

(defun my-theme-cycle ()
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (funcall (car theme-current))
  (message "%S" (car theme-current)))

(setq theme-current my-color-themes)
(setq color-theme-is-global nil) ; Initialization
(my-theme-set-default)
(global-set-key (kbd "C-c t") 'my-theme-cycle)

; colors
(custom-set-faces
 '(flymake-errline ((t :underline "red")))
 '(flymake-warnline ((t :underline "green"))))

; pretty diff-mode
(custom-set-faces
 '(diff-added ((t (:foreground "#559944"))))
 '(diff-context ((t nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-function ((t (:foreground "#00bbdd"))))
 '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
 '(diff-hunk-header ((t (:foreground "#fbde2d"))))
 '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
 '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
 '(diff-removed ((t (:foreground "#de1923")))))

; pretty magit diffs (based on colors for diff-mode above)
(set-face-attribute 'magit-diff-add nil :foreground "#559944")
(set-face-attribute 'magit-diff-del nil :foreground "#de1923")
(set-face-attribute 'magit-diff-file-header nil :foreground "RoyalBlue1")
(set-face-attribute 'magit-diff-hunk-header nil :foreground "#fbde2d")
(set-face-attribute 'magit-item-highlight nil :background "black")


