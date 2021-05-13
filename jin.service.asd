#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.service
  :description "Jin's lisp service."
  :version "1.0.0"
  :serial t
  :depends-on (
               :uiop
               :local-time
               :bordeaux-threads
               :cl-schedule                    ; Jin's cl-schedule v0.0.5
               )
  :components (
               (:file "package")
               (:file "service")
               (:file "with-cl-schedule")
               ))
