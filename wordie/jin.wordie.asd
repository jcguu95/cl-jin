#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.wordie
  :description "Lookup a word from a sentence. Save and announce the result."
  :version "1.0.0"
  :serial t
  :depends-on (:alexandria :trivial-clipboard
               :cl-ppcre :dbus :local-time :jin.utils)
  :components ((:file "package")
               (:file "wordie")))
