(in-package :formosa-bakery-helper)

(defun main (path)
  (defvar *target* nil)
  (setf *target* "~/full-test.csv")

  (defvar *data* nil)
  (setf *data*
        (cdr (cl-csv:read-csv (pathname *target*))))

  (defvar *record* nil)
  (mapcar #'column->record *data*) ;; Deconstructively update *record*!

  (apply #'concatenate 'string
         (mapcar #'order->string (mapcar #'cdr *record*))))
