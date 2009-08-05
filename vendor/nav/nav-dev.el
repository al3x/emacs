;;; nav-dev.el --- Key bindings etc. to help develop nav
;;
;; Copyright 2009 Google Inc. All Rights Reserved.
;;
;; Author: issactrotts@google.com (Issac Trotts)
;;

;; f7 runs nav's tests
(define-key emacs-lisp-mode-map [f7] (lambda ()
                                       (interactive)
                                       (load-file "nav-test.el")))
