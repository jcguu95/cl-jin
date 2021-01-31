(in-package :curfew)

;; kill app
(defun kill-app (app)
  (uiop:run-program '("pkill" app)))
