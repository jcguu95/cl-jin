(in-package :jin.utils)

(defun eval-in-emacs-synchronous- (s-expr)
  "Eval S-EXPR with `emacsclient' synchronously. This means that
it waits for emacs for an indefinitely long period of time."
  (let ((string (cl-ppcre:regex-replace-all
                 "jin.utils::?"
                 (write-to-string s-expr :case :downcase)
                 "")))
    (format t "Sending ~s to Emacs.~&" string)
    (read-from-string
     (with-output-to-string (stream)
       (uiop:run-program
        (list "emacsclient" "--eval" string) :output stream)))))

(defun eval-in-emacs (s-expr &key (timeout -1))
  "Evaluate s-expression with `emacsclient'."
  (if (minusp timeout)
      (eval-in-emacs-synchronous- s-expr)
      (ignore-errors
       (handler-case
           (trivial-timeout:with-timeout (timeout)
             (eval-in-emacs-synchronous- s-expr))
         (trivial-timeout:timeout-error (c)
           (declare (ignore c))
           (format t "EVAL-IN-EMACS time-outs after ~s seconds." timeout))))))

(defun ping-emacsclient (&key (timeout 0.2))
  (handler-case
      (trivial-timeout:with-timeout (timeout)
        (equal "ping" (eval-in-emacs "ping")))
    (trivial-timeout:timeout-error (c)
      (declare (ignore c))
      (format t "PING-EMACSCLIENT time-outs after ~s seconds." timeout))))
