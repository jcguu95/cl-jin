(in-package :curfew)

;; kill app
(defun kill-app (app)
  (format t "Killing app ~a.." app)
  (uiop:run-program '("pkill" app)))
