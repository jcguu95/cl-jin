(in-package :jin.utils)

(defun dmenu (candidates &optional prompt)
  "Receive a list CANDIDATES of strings with a optional PROMPT.
Let the user select a string using rofi or dmenu, and return the
selected string."
  (unless prompt (setf prompt ""))
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (out)
     (with-input-from-string
         (in (format nil "~{~a~%~}" candidates))
       (let ((rofi (whereis "rofi"))
             (dmenu (whereis "dmenu"))
             command args)
         (if rofi
             (progn (setf command rofi)
                    (setf args (list "-dmenu" "-p" prompt)))
             (progn (setf command dmenu)
                    (setf args (list "-p" prompt "-fn"
                                     (format nil "~a-~a" font size)))))
         (sb-ext:run-program command args
                             :input in
                             :output out
                             :error *standard-output*))))))
