; finally I want to do this for so long
;  --> curfew in common lisp!

(defvar *url-list*
  '("www.youtube.com" "news.ycombinator.com"
    "www.reddit.com" "old.reddit.com"
    "www.facebook.com" "www.messenger.com"
    "www.instagram.com"))

(defun main ()
  "Main entry point."
  ;; TODO Write a macro that clears this mess up.
  (when (funcall (time-within (1700 1730))
                 (now-in-int))
    (mapcar #'unlock-url *url-list&))
  (when (funcall (time-within (1730 1700))
                 (now-in-int))
    (mapcar #'lock-url *url-list*))
  ;; FIXME remember that lock-url has a weird bug.

  (when (funcall (time-within (1700 1900))
                 (now-in-int))
    (slock-all))
  (when (funcall (time-within (1930 0500))
                 (now-in-int))
    (slock-all)))


;<pjb> (defmacro ???? (&rest pairs) `(progn ,@(mapcar (lambda (pair) `(when
;      ,@pair)) pairs))) #| --> ???? |#
;<pjb> (macroexpand-1 '(???? (a1 a2) (b1 b2) (c1 c2) (d1 d2))) #| --> (progn
;      (when a1 a2) (when b1 b2) (when c1 c2) (when d1 d2)) ; t |#
;<pjb> (macroexpand-1 '(???? (a1 a2) (b1 b2))) #| --> (progn (when a1 a2) (when
;      b1 b2)) ; t |#
