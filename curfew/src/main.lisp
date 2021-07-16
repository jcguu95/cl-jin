(in-package :jin.curfew)

(setf *url-list*
      '(
        "www.youtube.com" "news.ycombinator.com"
        "www.reddit.com" "old.reddit.com"
        "www.facebook.com" "www.messenger.com" "www.instagram.com"
        ))

(setf *instruction*
      '(
        ((1930 0230) (kill-user "jin"))
        ((0230 0500) (slock-all "jin"))
        ((0230 1700) (continue-user "jin"))
        ((1700 1900) (kill-user "jin"))
        ((1900 1930) (continue-user "jin"))

        ((1700 1630) (mapcar #'lock-url *url-list*))
        ((1630 1700) (mapcar #'unlock-url *url-list*))
        ))

(defun main ()
  "Main entry point."
  (format t
	  "~%------~%Function #'main executed at ~a.~%------~%"
	  (local-time:now))
  (mapcar #'within=>do *instruction*)
  )
