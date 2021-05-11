(in-package :jin.utils)

;; TODO Use jin.service to launch dmenu.
(defun dmenu (candidates &optional prompt)
  (string-right-trim
   '(#\Newline)
   (with-output-to-string
       (out)
     (with-input-from-string
         (in (format nil "~{~a~%~}" candidates))
       (sb-ext:run-program (whereis "dmenu")
                           `("-p" ,prompt)
                           :input in :output out)))))
