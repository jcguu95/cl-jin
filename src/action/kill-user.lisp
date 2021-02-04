(in-package :curfew)

;; TODO testing
(defun kill-user (user)
  (format t "Suspending user ~a.." user)
  (uiop:run-program `("pkill" "-STOP" "-u" ,user)))

;; TODO testing
(defun continue-user (user)
  (format t "Continuing user ~a.." user)
  (uiop:run-program `("pkill" "-CONT" "-u" ,user)))
