(in-package :formosa-bakery-helper)

(defun column->*record* (column)
  "Update the *record* deconstructively from the given COLUMN."
  (when (metap column)
    (let ((new-order (make-order
                      :uuid  (nth 0 column)
                      :date  (nth 1 column)
                      :total (nth 7 column)
                      :name  (concatenate 'string
                                          (nth 14 column) " "
                                          (nth 15 column))
                      :phone (nth 23 column)
                      :entries ())))
      (push (cons (order-uuid new-order) new-order) *record*)))
  (when (entryp column)
    (let ((*order* (assoc (nth 0 column) *record* :test #'string=)))
      (push (make-entry :amount (nth 35 column)
                        :name (nth 33 column)
                        :variation (nth 34 column)
                        :unit-price (nth 36 column)
                        :price (nth 39 column))
            (order-entries (cdr *order*)))))
  ;; TODO otherwise, throw error!
  )

(defun order->string (order)
;; testing :
;; (order->string (cdr (car *record*)))
;; (mapcar #'order->string (mapcar #'cdr *record*))
  (concatenate 'string
          newline
          (order-uuid        order) " "
          "($" (order-total  order) ") "
          "[" (order-date    order) "]"
          ;newline
          " " (order-phone order)
          " " (order-name    order)
          newline
          (apply #'concatenate 'string
                 (mapcar #'entry->string (order-entries order)))))

(defun entry->string (entry)
;; testing :
;; (entry->string (car (order-entries (cdr (car *record*)))))
  (concatenate 'string
          (entry-amount     entry) "  "
          (entry-unit-price entry) " "
          (entry-price      entry) "  "
          (entry-name       entry) " -- "
          (entry-variation  entry) "  " newline))
