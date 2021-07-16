#-asdf3.1 (error "`jin' requires ASDF 3.1")

(asdf:defsystem "jin"
  ;; system attributes:
  :description "Jin's Scripts"
  :author "Jin <jcguu95@gmail.com>"
  :licence "MIT"
  ;; component attributes:
  :version "0.0"
  :properties ((#:author-email . "jcguu95@gmail.com"))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :depends-on ("jin.curfew"
               "jin.wordie"
               "jin.formosa-bakery-helper"
               ;; "jin.ledger-formatter" ;; not done
               "jin.questionnaire"
               "jin.utils"
               "jin.service"
               "jin.recording"
               )
  :components ())
