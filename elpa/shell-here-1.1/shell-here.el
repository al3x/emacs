;;; shell-here.el --- Open a shell relative to the working directory

;; Copyright (C) 2009-2012  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 1.1
;; Keywords: unix, tools, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (require 'shell-here)
;; (define-key (current-global-map) "\C-c!" 'shell-here)

;;; Code:

(eval-when-compile
  (require 'cl))

(defun shell-here-walk-up (base steps)
  (if (= steps 0) base
    (shell-here-walk-up (shell-here-stripslash
                         (file-name-directory base)) (- steps 1))))

(defun shell-here-stripslash (path)
  (if (and (> (length path) 1) (string= (substring path -1) "/"))
      (substring path 0 -1)
    path))

(defun shell-here-normalize (path)
  (when path (shell-here-stripslash (expand-file-name path))))

;;;###autoload
(defun shell-here (&optional arg)
  "Open a shell relative to `default-directory'.

With no argument, open a shell in `default-directory'.
With a positive numeric argument, open a shell ARG levels up from
`default-directory'.
With a plain negative argument, open a shell in the project root.
With a negative numeric argument, open a shell ARG levels up from the
project root.

Shell buffer names include the name of the current project's directory, if available; otherwise *shell*. If a shell buffer already exists, it will be reused.

With the universal argument, open a new shell in `default-directory'.
With a negative universal argument, open a new shell in the project
root.

Project root is determined with `ffip-project-root', if available."
  (interactive "P")
  (let* ((root-relative (< (prefix-numeric-value arg) 0))
         (new (consp arg))
         (levels (if (and arg (not (eq arg '-)) (not new))
                    (abs (prefix-numeric-value arg)) 0)))

    (when (and root-relative (not (fboundp 'ffip-project-root)))
      (unwind-protect
          (require 'find-file-in-project)
        (unless (fboundp 'ffip-project-root)
          (error "Find-file-in-project is required, but was not found."))))

    (let* ((root (shell-here-normalize (if (fboundp 'ffip-project-root)
                                           (ffip-project-root)
                                         default-directory)))
           (start (or (and root-relative root)
                      (shell-here-normalize default-directory)))
           (target (shell-here-walk-up start levels))

         (base-name
          (format "*shell%s*"
                  (if root (format " %s" (file-name-nondirectory root)) "")))
         (buf (pop-to-buffer
               (let ((default-directory (format "%s/" target)))
                 (cond
                  (new (generate-new-buffer base-name))
                  ((eq major-mode 'shell-mode) (current-buffer))
                  (t (get-buffer-create base-name)))))))

      (unless (let ((proc (get-buffer-process buf)))
                (and proc (process-live-p proc)))
        (shell buf))
      (goto-char (point-max))
      (when (not (string= (shell-here-stripslash
                           (expand-file-name default-directory)) target))
        (flet ((kill-region (start end)
                 (prog1
                     (buffer-substring start end) (delete-region start end))))
          (insert (prog1 (or (when (not new) (comint-kill-input)) "")
                    (insert (concat "cd " (shell-quote-argument target)))
                    (comint-send-input)
                    (shell-cd target))))))))

(provide 'shell-here)
;;; shell-here.el ends here
