(in-package :jin.curfew)

;; slock
(defun slock (user n)
  "Screen lock the N-th display as USER.

_Remark_ Depends on the binary slock."
  (let ((display n))
    (format t "Applying slock to DISPLAY=:~a~%" display)
    (uiop:run-program
     (format nil "su - ~a -c \"export DISPLAY=:~a ; slock &\"" user display))))

(defun slock-all (user)
  (loop for n in '(0 1 2 3 4 5 6 7 8 9)
        do (slock user n)))
