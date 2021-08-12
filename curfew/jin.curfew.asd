#-asdf3.1 (error "ASDF 3.1 or bust!")
(asdf:defsystem :jin.curfew
  :description "Self-control is an illusion."
  :version "1.0.0"
  :serial t
  :depends-on (:local-time)
  :components ((:file "package")
               (:file "src/aux")
               (:file "src/predicate")
               (:file "src/action/kill-app")
               (:file "src/action/kill-user")
               (:file "src/action/slock")
               (:file "src/action/url-lock")
               (:file "src/main")))
