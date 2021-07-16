#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.recording
  :description "Recording related shell scripts elevated into their lispy form."
  :version "1.0.0"
  :serial t
  :depends-on (:uiop :alexandria :jin.utils)
  :components ((:file "package")
               (:file "utils")
               (:file "recording")))
