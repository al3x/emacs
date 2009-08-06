;; This file will contain Bufs mode.

(defun nav-make-header (text)
  (propertize text
              'face
              '( :background "navy" :foreground "white")))


(defun nav-buffer-jump-button-action (button)
  (setq buf-name (button-label button))
  (other-window 1)
  (nav-buffer-jump buf-name))


(defun nav-buffer-jump (buf-name)
  "Jumps to selected buffer."
  (interactive)
  (get-buffer-create buf-name)
  (switch-to-buffer buf-name)
  (get-buffer buf-name))


(defun nav-bufs-make-mode-map ()
  "Creates and returns a mode map with bufs's key bindings."
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\t" 'forward-button)
    (define-key keymap "1" 'nav-open-buf-other-window-1)
    (define-key keymap "2" 'nav-open-buf-other-window-2)
    (define-key keymap "5" (lambda nil (interactive) (nav-quickfile-jump 0)))   
    (define-key keymap "6" (lambda nil (interactive) (nav-quickfile-jump 1)))
    (define-key keymap "7" (lambda nil (interactive) (nav-quickfile-jump 2)))
    (define-key keymap "r" 'nav-bufs-show-buffers)
    (define-key keymap "q" 'nav-quit)
    (define-key keymap "?" 'nav-help-screen)
    (define-key keymap "`" 'nav-bufs-quit)
    (define-key keymap [S-down-mouse-3] 'nav-bufs-quit)
    (define-key keymap [(control ?x) (control ?f)] 'find-file-other-window)
    keymap))

(setq nav-bufs-mode-map (nav-bufs-make-mode-map))

(defun nav-open-buf-other-window (k)
  (let ((filename (nav-get-cur-line-str))
        (dirname (nav-get-working-dir)))
    (other-window k)
    (nav-buffer-jump filename)))


(defun nav-open-buf-other-window-1 ()
  "Opens the file under the cursor in the first other window.

This is equivalent to just pressing the [enter] key. 
See nav-open-file-other-window-2."
  (interactive)
  (nav-open-buf-other-window 1))


(defun nav-ensure-second-window-exists ()
  "Makes sure there is a second file-editing area on the right.

Jumps back to nav window when done."
  (when (= 2 (length (window-list)))
    (other-window 1)
    (if (eq nav-split-window-direction 'horizontal)
        (split-window-horizontally)
      (split-window-vertically))
    (select-window (nav-get-window nav-buffer-name))))


(defun nav-open-buf-other-window-2 ()
  "Opens the file under the cursor in the second other window.

If there is no second other window, Nav will create one."
  (interactive)
  (nav-ensure-second-window-exists)
  (nav-open-buf-other-window 2))


(defun nav-bufs-show-buffers ()
  "Displays current buffers and create buttons to switch too."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq blist (mapcar (function buffer-name) (buffer-list)))
    (insert (nav-make-header "Active Buffers:     " ))
    (insert "\n")
    (dolist (b blist)
      (when (not (string-match "^[ *]" b))
	(insert-text-button b :type 'buffer-jump-button)
	(insert "\n")))
    (insert "\n")
    (insert (nav-make-header "Scratch Buffers:    "))
    (insert "\n")
    (dolist (b blist)
      (when (string-match "^\\*" b)
        (insert-text-button b :type 'buffer-jump-button)
        (insert "\n")))
    (setq mode-line-format "nav: Buffer list")
    (force-mode-line-update))
  (goto-line 2))


(defun nav-bufs-quit ()
  "Kill nav-bufs."
  (interactive)
  (nav-mode))


(define-derived-mode nav-bufs-mode fundamental-mode 
  "Nav-buf-mode is displaying and switching buffers."
  (setq mode-name "Nav buffers")
  (use-local-map nav-bufs-mode-map)
  (turn-on-font-lock)
  (setq buffer-read-only t)
  (nav-bufs-show-buffers))


(defun nav-bufs ()
  "Run nav-buf-mode on top of nav."
  (interactive)
  (nav-bufs-mode))


(provide 'nav-bufs)

;;; nav-bufs.el ends here