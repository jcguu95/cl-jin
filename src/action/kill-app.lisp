(in-package :curfew)

;; TODO untested
(defun kill-app (app)
  (format t "Killing app ~a.." app)
  (uiop:run-program '("pkill" app)))
