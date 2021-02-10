#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :dictionary
  :description "Lookup a word from a sentence. Save and announce the result."
  :version "1.0.0"
  :serial t
  :depends-on (:arrows :alexandria :trivial-clipboard
               :cl-ppcre :dbus :local-time :jin)
  :components ((:file "package")
               (:file "dictionary")))
