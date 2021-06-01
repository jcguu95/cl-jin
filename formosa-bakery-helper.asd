#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :formosa-bakery-helper
  :description "Format Square Space csv file."
  :version "1.0.0"
  :serial t
  :depends-on (:cl-csv)
  :components ((:file "package")
               (:file "aux")
               (:file "structure")
               (:file "data-manu")
               (:file "main")))
