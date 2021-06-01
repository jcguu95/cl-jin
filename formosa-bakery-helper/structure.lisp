(in-package :formosa-bakery-helper)

(defstruct
                                        ;(
    order
                                        ;(:print-function
                                        ;   (lambda (struct stream depth)
                                        ;     (declare (ignore depth))
                                        ;     (format stream "~D ~A ~D ~D ~D ~D"
                                        ;             (order-uuid struct)
                                        ;             (order-date struct)
                                        ;             (order-total struct)
                                        ;             (order-name struct)
                                        ;             (order-phone struct)
                                        ;             (order-entries struct))))
                                        ;)
  uuid date total name phone entries)

(defstruct
                                        ;(
    entry
                                        ;(:print-function
                                        ; (lambda (struct stream depth)
                                        ;   (declare (ignore depth))
                                        ;   (format stream "~D ~A ~D ~D~%~D"
                                        ;           (entry-amount struct)
                                        ;           (entry-name struct)
                                        ;           (entry-variation struct)
                                        ;           (entry-unit-price struct)
                                        ;           (entry-price struct))))
                                        ;)
  amount name variation unit-price price)
