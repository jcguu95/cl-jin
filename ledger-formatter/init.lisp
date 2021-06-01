(in-package :jin.ledger-formatter)

(defstruct ledger-entry
  date description comment flows)
(defstruct flow
  account unit amount)

(defun account (&rest args)
  ;; an aux function
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
  "The structure creator for chase credit 3869."
  ;; It should be abstractified later when more creators are written.
  (flet ((format-date (str)
           (format nil "~a-~a-~a"
                   (str:substring 6 10 str)
                   (str:substring 0 2 str)
                   (str:substring 3 5 str))))
    (make-ledger-entry
     :date (format-date (nth 0 row))
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
            (- (parse-float:parse-float (nth 5 row))))))))

;; chase 7685 has the same parser as 3869
(setf (symbol-function 'Chase-7685---row->ledger-entry)
      #'chase-3869---row->ledger-entry)
;; FIXME.. oh no the make-flow isn't the same.. so I really need
;; to write an abstraction for everyone. later.

(defun Chase-5688---row->ledger-entry (row)
  "The structure creator for chase checking 5688."
  (flet ((format-date (str)
           (format nil "~a-~a-~a"
                   (str:substring 6 10 str)
                   (str:substring 0 2 str)
                   (str:substring 3 5 str))))
    (make-ledger-entry
     :date (format-date (nth 1 row))
     :description (nth 2 row)
     :comment (nth 8 row)
     :flows
     (list (make-flow
            :account
            (account "Assets"
                     "Checking"
                     "Chase Checking - Ending in 5688")
            :unit "USD"
            :amount
            (parse-float:parse-float (nth 3 row)))
           (make-flow
            :account (tag->account (nth 7 row))
            :unit "USD"
            :amount
            (- (parse-float:parse-float (nth 3 row))))))))

(defun Boa-7204---row->ledger-entry (row)
  "The structure creator for boa saving checking 7204."
  (flet ((format-date (str)
           (format nil "~a-~a-~a"
                   (str:substring 6 10 str)
                   (str:substring 0 2 str)
                   (str:substring 3 5 str))))
    (make-ledger-entry
     :date (format-date (nth 0 row))
     :description (nth 1 row)
     :comment (nth 5 row)
     :flows
     (list (make-flow
            :account
            (account "Assets"
                     "Saving"
                     "BofA Saving - Ending in 7204")
            :unit "USD"
            :amount
            (parse-float:parse-float (nth 2 row)))
           (make-flow
            :account (tag->account (nth 4 row))
            :unit "USD"
            :amount
            (- (parse-float:parse-float (nth 2 row))))))))

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
    (""  (account "Other" "Unknown" "UNDONE"))
    ;; --- Ignore
    ("IGNORE" "")
    ("e"      "")))


;;;  formatters!
;;;
(defun format-ledger-entry-flow (ledger-entry-flow)
  (format nil "~a    ~a ~a"
          (flow-account ledger-entry-flow)
          (flow-unit ledger-entry-flow)
          (flow-amount ledger-entry-flow)))
(defun format-ledger-entry (ledger-entry)
  "Returns a formatted ledger entry."
  (let ((i ledger-entry))
    (format nil "~a~%~a"
            (format nil "~a ~a~%;; ~a"
                    (ledger-entry-date i) ;; FIXME need a timestamp-reformatter
                    ;; TODO lemme learn how full-fledged parser combinators work and come back to this
                    ;; Or I should just us `rx` as in elisp... haven't check `cl-ppcre`
                    (ledger-entry-description i)
                    (ledger-entry-comment i))
            (format nil "~{~a~%~}"
                    (mapcar #'format-ledger-entry-flow
                            (ledger-entry-flows i))))))

;; TEMPLATE
;; 2020-06-30 Payment Received - Thank You
;;   ;;
;;   Liabilities:Credit Cards:AmEx Delta Skymiles Gold Card X1009    USD 7.49
;;   Other:Unknown:Duplicate

;;; testing zone
(defvar amex-21009 "./data/amex-credit-21009.sample.csv")
(defvar amex-71008 "./data/amex-credit-71008.sample.csv")
(defvar boa-7204 "./data/boa-saving-7204.sample.csv")
(defvar chase-5688 "./data/chase-checking-5688.sample.csv")
(defvar chase-3869 "./data/chase-credit-3869.sample.csv")
(defvar chase-7685 "./data/chase-credit-7685.sample.csv")
(defvar cs-5372 "./data/charles-schwab-checking-5372.sample.csv")
(defvar cs-2737 "./data/charles-schwab-individual-2737.sample.csv")

(setf *data* (cdr (csv->rows chase-3869)))
(setf tmp/entries (mapcar #'Chase-3869---row->ledger-entry *data*))
