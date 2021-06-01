(in-package :jin.utils)

(defun whereis (target)
  (string-right-trim
   (format nil "~%")
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "whereis ~a | cut -d' ' -f2" target)
      ;; TODO make this return multiple values
      :output out))))
