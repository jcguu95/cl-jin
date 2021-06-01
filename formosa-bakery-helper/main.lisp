(in-package :jin.formosa-bakery-helper)

(defun main (&key input (output "/tmp/test.txt"))
  "Entry function of the project."
  (let ((data (cdr (cl-csv:read-csv (pathname input)))))

    ;; Deconstructively update *record*!
    (setf *record* nil)
    (mapcar #'column->*record* data)

    ;; *record* -> string
    (let* ((strs (mapcar #'order->string (mapcar #'cdr *record*)))
           (str (apply #'concatenate 'string strs)))
      ;; Showcase the result.
      str
      (format t "~a pending order in total." (length *record*))
      ;; Write string to OUTPUT.
      (when output
        (progn
          (with-open-file (f (pathname output)
                             :direction :output
                             :if-exists :overwrite
                             :if-does-not-exist :create)
            (write-sequence str f)))))))
