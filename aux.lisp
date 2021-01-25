(in-package :formosa-bakery-helper)

(defvar newline
  (make-string 1 :initial-element #\Newline))

(defun metap (column)
  "Check whether the given column consists of the meta data of an
order."
  (and (equalp (nth 2 column) "pending")
       (equalp (nth 3 column) "USD")))

(defun entryp (column)
  "Check whether the given column consists an entry data."
  (and (equalp (nth 2 column) "")
       (equalp (nth 3 column) "")))
