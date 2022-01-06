(in-package :jin.utils)

(defun eval-in-emacs (&rest s-exprs)
  "Evaluate s-expressions with `emacsclient'."
  (let ((string (write-to-string
                 `(progn ,@s-exprs) :case :downcase)))
    (format t "Sending ~s to Emacs." string)
    (ignore-errors (uiop:run-program
                   (list "emacsclient" "--eval" string)))))
