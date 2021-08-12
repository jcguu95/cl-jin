(in-package :jin.curfew)

(setf *dns-path* "/etc/hosts")

(defun lock-url (url)
  "Uncomment the first DECO#URL if any in *DNS-PATH*. Otherwise,
append DECO-URL to *DNS-PATH* if there are no DECO-URL nor
DECO#URL."
  (let* ((deco-url (format nil "127.0.0.1 ~a  # curfew.d" url))
         (deco#url (format nil "#127.0.0.1 ~a  # curfew.d" url))
         (dns-file (get-file *dns-path*)))
    (if (find deco-url dns-file :test #'equal)
        (progn
          ;; (format t "Already locked:~% URL=~a~%" url)
          nil)
        (let ((result (if (find deco#url dns-file :test #'equal)
                          (substitute deco-url deco#url
                                      dns-file :test #'equal)
                          (append dns-file (list deco-url)))))
          (with-open-file (stream *dns-path*
                                  :direction :output
                                  :if-exists :overwrite)
            (format stream "~{~a~^~%~}" result))
          ;; FIXME The writing action above has a bug. It
          ;; duplicates the last letter of the last line.
          nil
          ;; (format t "Locking URL: \"~a\".~%" url)
          ))))

(defun unlock-url (url)
  "In DNS-FILE, comment the URL whenever there is DECO-URL."
  (let* ((deco-url (format nil "127.0.0.1 ~a  # curfew.d" url))
         (deco#url (format nil "#127.0.0.1 ~a  # curfew.d" url))
         (dns-file (get-file *dns-path*)))
    (if (find deco-url dns-file :test #'equal)
        (let ((result (substitute deco#url deco-url
                                  dns-file :test #'equal)))
          (with-open-file (stream *dns-path*
                                  :direction :output
                                  :if-exists :overwrite)
            (format stream "~{~a~^~%~}" result))
          ;; (format t "Unlocking URL: \"~a\".~%" url)
          )
        ;; (format t "URL is not locked by this program:~% URL=~a~%" url)
        )))
