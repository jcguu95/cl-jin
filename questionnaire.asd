#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :questionnaire
  :description "Ask questions and record answers."
  :version "1.0.0"
  :serial t
  :depends-on (
               :alexandria
               :cl-csv
               :cl-date-time-parser
              )
  :components (
               (:file "package")
               (:file "aux-util")
               (:file "main-util")
               (:file "main")
               (:file "config")
	      ))
