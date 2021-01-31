(in-package :curfew)

(defvar *dns-path* "/home/jin/hosts")
;(defvar *dns-path* "/etc/hosts")

(defun lock-url (url)
  "In DNS-FILE, do the following. If there is DECO-URL, then
mention that it's locked already by this program. If there is not
DECO-URL but there is DECO#URL, uncomment the first DECO#URL, and
mention the change. If there are no DECO-URL nor DECO#URL, append
DECO-URL to *DNS-PATH*, and mention the change.

_Remark_ This function may require root privilege to run.

_Remark_ This function depends on the global variable *DNS-PATH*."
  (let* ((deco-url (format nil "127.0.0.1 ~a  # curfew.d" url))
         (deco#url (format nil "#127.0.0.1 ~a  # curfew.d" url))
         (dns-file (get-file *dns-path*)))
    (if (find deco-url dns-file :test #'equal)
        (format t "URL locked already by this program: \"~a\"." url)
        (let ((result (if (find deco#url dns-file :test #'equal)
                          (substitute deco-url deco#url
                                      dns-file    :test #'equal)
                          (append dns-file (list deco-url)))))
          ;; mention it, and write result to file.
          (with-open-file (stream *dns-path*
                                  :direction :output
                                  :if-exists :overwrite)
            (format stream "~{~a~^~%~}" result))
          ;;; FIXME the writing action above has a weird bug.
          ;;; It duplicates the last letter of the last line.
          ;;; It doesn't happen to the function #'unlock-url below.
          ;;; I don't know why.
          (format t "Locking URL: \"~a\"." url)))))

(defun unlock-url (url)
  "In DNS-FILE, when there is DECO-URL, uncomment the URL and
mention it. Otherwise, mention that it is not locked by this
program.

_Remark_ This function may require root privilege to run.

_Remark_ This function depends on the global variable *DNS-PATH*."
  (let* ((deco-url (format nil "127.0.0.1 ~a  # curfew.d" url))
         (deco#url (format nil "#127.0.0.1 ~a  # curfew.d" url))
         (dns-file (get-file *dns-path*)))
    (if (find deco-url dns-file :test #'equal)
      (let ((result (substitute deco#url deco-url
                                dns-file :test #'equal)))
        ;; write result to file, and mention it.
        (with-open-file (stream *dns-path*
                                :direction :output
                                :if-exists :overwrite)
          (format stream "~{~a~^~%~}" result))

          (format t "Unlocking URL: \"~a\"." url))

      (format t "URL not locked by this program: \"~a\"." url))))
