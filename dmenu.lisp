(in-package :jin.utils)

;; TODO Use jin.service to launch dmenu.
(defun dmenu (candidates &optional prompt)
  (string-right-trim
   '(#\Newline)
   (with-output-to-string
       (out)
     (with-input-from-string
         (in (format nil "~{~a~%~}" candidates))
       (let ((font "terminus")
             (size "18"))
         (sb-ext:run-program (whereis "dmenu")
                             (list "-p" prompt "-fn"
                                   (concatenate 'string
                                                font "-" size))
                             :input in :output out))))))
