;; Save-visited-files.el - save opened files across sessions

;; Copyright (C) 2009 Nathaniel Flath <nflath@email.com>
;; Version: 1.0
;;
;; Commentary Save-visited-files is a lightweight version of
;; Desktop.el that only save the files you have open(currently).  This
;; was created because I couldn't ever get Desktop to work and wanted
;; to persist open files across sessions.  This file is the result.
;;
;; To install, put this file somewhere in your load-path and add the
;; following to your .emacs file:
;;
;; (require 'savebuffers)
;; (open-visited-files)
;; (auto-save-visited-files 1)
;;
;; This will load the set of saved files on startup, as well as
;; updating this list whenever a file is opened or a buffer is closed.
;; This does not wait to save on closing emacs because I wanted it to
;; be useful even if emacs crashed.  To save the visited files at any
;; time, you can call M-x save-visited-files.  M-x Open-visited-files
;; will open all files saved this way.  To turn off the saving of
;; files, you need to run (save-visited-files t).
;;
;;
;; Changelog:
;; 1.0
;;  * Initial Release
;;
;; Code:

(defvar save-visited-files-location "~/.emacs.d/.emacs-opened-files"
  "Location of the file that contains the list of previously visited files")
(defvar save-visited-files-interval 60
  "How oftern, in seconds, to save opened files")

(defvar save-visited-files-timer nil
  "timer controlling save-visited-files")

(defun save-visited-files ()
  "Saves the location of all open files to be re-opened when
open-visited-files is called."
  (interactive)
  (let ((buf (current-buffer)))
    (unless (string-equal (buffer-name buf) " *Minibuf-1*")
      (switch-to-buffer "*Save Visited*")
      (erase-buffer)
      (mapcar '(lambda (x) (insert x "\n"))
              (remove-if '(lambda (x)
                            (or (string-equal save-visited-files-location x)
                                (eq nil x))) (mapcar 'buffer-file-name (buffer-list))))
      (write-file save-visited-files-location nil)
      (kill-buffer (get-buffer "*Save Visited*"))
      (switch-to-buffer buf))))

(defun open-visited-files ()
  "Opens all files that were saved by save-visited-files."
  (interactive)
  (find-file save-visited-files-location)
  (beginning-of-buffer)
  (let ((buf (current-buffer)))
    (while (not (eq (point) (point-max)))
      (let ((point (point)))
        (end-of-line)
        (find-file (buffer-substring point (point)))
        (switch-to-buffer buf)
        (next-line)
        (beginning-of-line)))
    (kill-buffer buf)))

(defun save-visited-files-mode (&optional arg)
  "If arg is non-nil, start saving opened files on find-file and
kill-buffer.  Otherwise, turn it off.  With no arg, toggle
save-visited-files-mode."
  (interactive)
  (cond
   ((eq nil arg) (if (not save-visited-files-timer)
                     (setq save-visited-files-timer (run-with-timer 0 60 'save-visited-files))
                   (progn
                     (cancel-timer save-visited-files-timer)
                     (setq save-visited-files-timer nil))))
   ((> arg 0)  (when (not save-visited-files-timer)
                 (setq save-visited-files-timer (run-with-timer 0 60 'save-visited-files))))
   ((< arg 0)  (when (save-visited-files-timer)
                 (cancel-timer save-visited-files-timer)
                 (setq save-visited-files-timer nil)))))

(provide 'save-visited-files)