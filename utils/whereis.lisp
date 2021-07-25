(in-package :jin.utils)

(defun whereis (target)
  (let ((result (string-right-trim
                 (format nil "~%")
                 (with-output-to-string (out)
                   (uiop:run-program
                    (format nil "whereis ~a | cut -d' ' -f2" target)
                    ;; TODO make this return multiple values
                    :output out)))))
    ;; if exists it must start with the character #\/
    (unless (eq #\/ (char result 0)) (setf result nil))
    result))
