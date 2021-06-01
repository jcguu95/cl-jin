(in-package :jin.formosa-bakery-helper)

(setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)

;; The variable that gets updated
;; deconstructively and holds the orders.
(defvar *record*)
(setf *record* nil)

(defvar newline
  (make-string 1 :initial-element #\Newline))

(defun metap (column)
  "Check whether the given column consists of the meta data of an
order."
  (and (equalp (nth 2 column) "pending") ;; TODO some entries are cancelled, and will raise error.
       (equalp (nth 3 column) "USD")))

(defun entryp (column)
  "Check whether the given column consists an entry data."
  (and (equalp (nth 2 column) "")
       (equalp (nth 3 column) "")))
