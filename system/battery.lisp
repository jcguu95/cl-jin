(in-package :jin)

(defun echo-battery-state ()
  (format nil "(BAT~{~% ~a~})" (car (trivial-battery:battery-info))))
