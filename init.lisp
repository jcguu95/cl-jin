(ql:quickload :alexandria)
(ql:quickload :arrows)
(ql:quickload :cl-csv)
(ql:quickload :parse-float)

(defstruct ledger-entry date description comment flows)
(defstruct flow account unit amount)

;; aux
(defun account (&rest args)
  "Expect each arg to be a string, interpose with colons, and
concatenate."
  (arrows:->> args
              (mapcar (lambda (e) (cons ":" e)))
              (alexandria:flatten)
              (cdr)
              (apply #'concatenate 'string)))

(defun csv->rows (csv)
  (cl-csv:read-csv (pathname csv)))

(defun Chase-3869---row->ledger-entry (row)
  "TODO"
  (make-ledger-entry
   :date (nth 0 row)
   :description (nth 2 row)
   :comment (nth 6 row)
   :flows
   (list (make-flow
          :account
          (account "Liabilities"
                         "Credit Cards"
                         "Chase Credit Card - Ending in 3869")
          :unit "USD"
          :amount
          (parse-float:parse-float (nth 5 row)))
         (make-flow
          :account (tag->account (nth 7 row))
          :unit "USD"
          :amount
          (- (parse-float:parse-float (nth 5 row)))))))

(defun tag->account (tag)
  "A look-up table."
  (alexandria:eswitch (tag :test #'equal)
    ;; --- Generic
    ("F" (account "Expenses" "Food"))
    ("T" (account "Expenses" "Transportation"))
    ("H" (account "Expenses" "Housing"))
    ("O" (account "Expenses" "Other"))
    ("t" (account "Expenses" "Tax"))
    ("i" (account "Assets" "Investment"))
    ("I" (account "Income" "Interest"))
    ("s" (account "Income" "Salary"))
    ;; --- Unknown
    ("c" (account "Other" "Unknown" "Cash"))
    ("d" (account "Other" "Unknown" "Duplicate"))
    ("?" (account "Other" "Unknown" "Undetermined"))
    ("!" (account "Other" "Unknown" "Mixed"))
    ;; --- Ignore
    ("IGNORE" "")
    ("e"      "")))

(defun ledger-entry->out (ledger-entry)
  "Returns a formatted ledger entry."
  ;TODO
  )

;;; testing zone
;;; testing zone
(setf *data* (cdr (csv->rows "./data/Chase3869_sample.CSV")))
(mapcar #'Chase-3869---row->ledger-entry *data*)
;;; testing zone
;;; testing zone
