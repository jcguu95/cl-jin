#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :curfew
  :description "Self-control is an illusion."
  :version "1.0.0"
  :serial t
  :depends-on (:local-time)
  :components ((:file "package")
               (:file "aux")
               (:file "predicate")
               (:file "action/kill-app")
               (:file "action/slock")
               (:file "action/url-lock")
               (:file "main")))
