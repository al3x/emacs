(defun chomp (str)
     "..."
     (let ((s (if (symbolp str)(symbol-name str) str)))
        (save-excursion
          (while (and
                  (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
                  (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
            (setq s (replace-match "" t nil s)))
          (while (and
                  (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
                  (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
            (setq s (replace-match "" t nil s))))
        s))