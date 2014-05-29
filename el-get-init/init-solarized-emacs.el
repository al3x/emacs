(setq my-themes '(solarized-dark solarized-light))

(setq my-cur-theme nil)
(defun cycle-my-theme ()
  "Cycle through a list of preferred themes"
  (interactive)
  (when my-cur-theme
    (disable-theme my-cur-theme)
    (setq my-themes (append my-themes (list my-cur-theme))))
  (setq my-cur-theme (pop my-themes))
  (load-theme my-cur-theme t))

;; switch to the first theme in the list above: solarized-dark
(cycle-my-theme)

;; bind theme switching to F12
(global-set-key (kbd "<f12>") 'cycle-my-theme)
