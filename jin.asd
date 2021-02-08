#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin
  :description "Jin's scripts."
  :version "1.0.0"
  :serial t
  :depends-on (:uiop :alexandria)
  :components ((:file "package")
               (:file "dmenu")))
