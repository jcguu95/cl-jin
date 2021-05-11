#-asdf3.1 (error "ASDF 3.1 or bust!")

(asdf:defsystem :jin.utils
  :description "Jin's scripts."
  :version "1.0.0"
  :serial t
  :depends-on (:uiop :alexandria :dbus :cl-ppcre :stumpwm
                     :trivial-battery :jin.service)
  :components ((:file "package")
               (:file "dmenu")
               (:file "notify-send")
               (:file "org-timestamp")
               (:file "cpu")
               (:file "wifi")
               (:file "battery")
               (:file "brightness")
               (:file "audio-volume")
               ))
