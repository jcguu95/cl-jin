#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.utils
  :description "Jin's scripts."
  :version "1.0.0"
  :serial t
  :depends-on (:uiop :alexandria :dbus :cl-ppcre :stumpwm :fiveam
                     :trivial-battery :jin.service :trivial-timeout)
  :components ((:file "package")
               (:file "whereis")
               (:file "select")
               (:file "notify-send")
               (:file "org-timestamp")
               (:file "cpu")
               (:file "wifi")
               (:file "battery")
               (:file "brightness")
               (:file "audio-volume")
               (:file "transparency")
               (:file "eval-in-emacs")
               (:file "lexic-search")
               (:file "format-sexpr")
               ))
