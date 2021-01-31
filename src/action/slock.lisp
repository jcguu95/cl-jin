(in-package :curfew)

;; slock
(defun slock (n)
  "Screen lock the N-th display.

_Remark_ Depends on the binary slock."
  (let ((display n))
    (format t "Applying slock to DISPLAY=:~a" display)
    (uiop:run-program
     (format nil "export DISPLAY=:~a ; slock &" display))))

(defun slock-all ()
  (loop for n in '(0 1 2 3 4 5 6 7 8 9)
        do (slock n)))
