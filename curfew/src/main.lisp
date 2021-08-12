(in-package :jin.curfew)

(setf *url-list*
      '(
        "www.youtube.com" "news.ycombinator.com"
        "www.reddit.com" "old.reddit.com"
        "www.facebook.com" "www.messenger.com" "www.instagram.com"
	"chat.freenode.net"
        ))

(setf *instruction*
      '(
        ((2030 0230) (kill-user "jin"))
       ;((0230 0500) (slock-all "jin"))
       ;((0230 1700) (continue-user "jin"))
       ;((1700 1900) (kill-user "jin"))
        ((0230 2030) (continue-user "jin"))

       ;((1930 1630) (mapcar #'lock-url *url-list*))
       ;((1630 1700) (mapcar #'unlock-url *url-list*))
       ;((1700 1900) (mapcar #'lock-url *url-list*))
       ;((1900 1930) (mapcar #'unlock-url *url-list*))

        ((0000 0050) (mapcar #'lock-url *url-list*))
        ((0100 0150) (mapcar #'lock-url *url-list*))
        ((0200 0250) (mapcar #'lock-url *url-list*))
        ((0300 0350) (mapcar #'lock-url *url-list*))
        ((0400 0450) (mapcar #'lock-url *url-list*))
        ((0500 0550) (mapcar #'lock-url *url-list*))
        ((0600 0650) (mapcar #'lock-url *url-list*))
        ((0700 0750) (mapcar #'lock-url *url-list*))
        ((0800 0850) (mapcar #'lock-url *url-list*))
        ((0900 0950) (mapcar #'lock-url *url-list*))
        ((1000 1050) (mapcar #'lock-url *url-list*))
        ((1100 1150) (mapcar #'lock-url *url-list*))
        ((1200 1250) (mapcar #'lock-url *url-list*))
        ((1300 1350) (mapcar #'lock-url *url-list*))
        ((1400 1450) (mapcar #'lock-url *url-list*))
        ((1500 1550) (mapcar #'lock-url *url-list*))
        ((1600 1650) (mapcar #'lock-url *url-list*))
        ((1700 1750) (mapcar #'lock-url *url-list*))
        ((1800 1850) (mapcar #'lock-url *url-list*))
        ((1900 1950) (mapcar #'lock-url *url-list*))
        ((2000 2050) (mapcar #'lock-url *url-list*))
        ((2100 2150) (mapcar #'lock-url *url-list*))
        ((2200 2250) (mapcar #'lock-url *url-list*))
        ((2300 2351) (mapcar #'lock-url *url-list*))

        ((0050 0100) (mapcar #'unlock-url *url-list*))
        ((0150 0200) (mapcar #'unlock-url *url-list*))
        ((0250 0300) (mapcar #'unlock-url *url-list*))
        ((0350 0400) (mapcar #'unlock-url *url-list*))
        ((0450 0500) (mapcar #'unlock-url *url-list*))
        ((0550 0600) (mapcar #'unlock-url *url-list*))
        ((0650 0700) (mapcar #'unlock-url *url-list*))
        ((0750 0800) (mapcar #'unlock-url *url-list*))
        ((0850 0900) (mapcar #'unlock-url *url-list*))
        ((0950 1000) (mapcar #'unlock-url *url-list*))
        ((1050 1100) (mapcar #'unlock-url *url-list*))
        ((1150 1200) (mapcar #'unlock-url *url-list*))
        ((1250 1300) (mapcar #'unlock-url *url-list*))
        ((1350 1400) (mapcar #'unlock-url *url-list*))
        ((1450 1500) (mapcar #'unlock-url *url-list*))
        ((1550 1600) (mapcar #'unlock-url *url-list*))
        ((1650 1700) (mapcar #'unlock-url *url-list*))
        ((1750 1800) (mapcar #'unlock-url *url-list*))
        ((1850 1900) (mapcar #'unlock-url *url-list*))
        ((1950 2000) (mapcar #'unlock-url *url-list*))
        ((2050 2100) (mapcar #'unlock-url *url-list*))
        ((2150 2200) (mapcar #'unlock-url *url-list*))
        ((2250 2300) (mapcar #'unlock-url *url-list*))
        ((2350 0000) (mapcar #'unlock-url *url-list*))
        ))

(defun main ()
  "Main entry point."
;  (format t
;	  "~%------~%Function #'main executed at ~a.~%------~%"
;	  (local-time:now))
  (mapcar #'within=>do *instruction*)
  )
