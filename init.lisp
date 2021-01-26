(ql:quickload :cl-csv)

(defun csv->columns (csv)
  (cl-csv:read-csv (pathname csv)))

(defun column->struct (template column)
  "Template must be customized for each different csv."
  (print "This function hasn't been implemented."))

(defun struct->out (struct)
  "Final formatter that returns a ledger entry.")

(defstruct ledger-entry
  date description comment flows)

(defstruct flow
  account unit amount)

;; template for Chase 3869
;; perhaps should use plist TODO
(0 . '(id date))
(2 . '(id description))
(5 . '(id amount))
(6 . '(id comment))
(7 . '((category -> "a"-flow))) ;; todo
