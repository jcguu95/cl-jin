(defpackage jin.utils
  (:use :cl)
  (:export

   :dmenu
   :whereis
   :notify-send
   :notify-send-using-dbus
   :org-timestamp

   :echo-cpu-state
   :echo-wifi-state
   :echo-battery-state

   :set-brightness

   :alter-audio-volume
   :toggle-mute-audio
   :normalize-audio
   :audio-volume

   :set-opacity
   :alter-opacity

   :eval-in-emacs
   ))
