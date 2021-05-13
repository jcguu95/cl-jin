#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.service
  :description "Jin's lisp service."
  :version "1.0.0"
  :serial t
  :depends-on (
               :uiop
               :local-time
               :bordeaux-threads
               :clon                    ; Jin's Clon v.0.0.3
               )
  :components (
               (:file "package")
               (:file "service")
               (:file "with-clon")
               ))
