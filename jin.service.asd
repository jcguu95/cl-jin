#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.service
  :description "Jin's lisp service."
  :version "1.0.0"
  :serial t
  :depends-on (:uiop :local-time :bt-semaphore)
  :components ((:file "package")
               (:file "service")
               (:file "example")
               ))
