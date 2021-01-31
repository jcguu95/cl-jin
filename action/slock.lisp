(in-package :curfew)

;; slock
(defun slock (n)
  "Screen lock the N-th display.

_Remark_ Depends on the binary slock."
  (let ((display n))
    (uiop:run-program
     (format nil "export DISPLAY=:~a ; slock &" display))))
