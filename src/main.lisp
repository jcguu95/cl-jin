(in-package :curfew)

(setf *url-list*
      '(
        "www.youtube.com" "news.ycombinator.com"
        "www.reddit.com" "old.reddit.com"
        "www.facebook.com" "www.messenger.com" "www.instagram.com"
        ))

(setf *instruction*
      '(
        ((1700 1900) (slock-all "jin"))
        ((1930 0500) (slock-all "jin"))

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
