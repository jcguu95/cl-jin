(in-package :jin.utils)

(defun dmenu-deprecated (candidates &optional prompt)
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (out)
     (with-input-from-string
         (in (format nil "~{~a~%~}" candidates))
       (let ((font "terminus")
             (size "18"))
         (sb-ext:run-program (whereis "dmenu")
                             (list "-p" prompt "-fn"
                                   (concatenate 'string
                                                font "-" size))
                             :input in :output out))))))

(defun dmenu (candidates &optional prompt)
  (unless prompt (setf prompt ""))
  (let* ((command (whereis "rofi"))
         (service (make-instance
                   'jin.service:service
                   :name "rofi"
                   :action `(lambda ()
                              (with-output-to-string (out)
                                (with-input-from-string
                                    (in ,(format nil "~{~a~%~}" candidates))
                                  (sb-ext:run-program
                                   ,command
                                   ',(list "-dmenu" "-p" prompt)
                                   :input in
                                   :output out
                                   :error *standard-output*)))))))
    (jin.service:dispatch service)))
