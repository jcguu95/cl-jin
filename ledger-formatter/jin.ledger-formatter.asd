#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.ledger-formatter
  :description "Jin's ledger formatter for financial transactions."
  :version "1.0.0"
  :serial t
  :depends-on (:alexandria :arrows :cl-csv :str :parse-float)
  :components ((:file "package")
               (:file "init")
               ))
