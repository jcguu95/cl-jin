(in-package :questionnaire)

(defun aux/interpose (sep lst)
  ;; an aux list operation
  "For example, (aux/interpose 7 '(1 2 3)) returns (1 7 2 7 3)."
  (cdr (alexandria:flatten
    (mapcar (lambda (e) (cons sep e)) lst))))

(defun aux/format-time-string-now ()
  "An auxiliary time string formatter. It returns a string that
indicates the current time. For example, \"2021-01-24
11:28:15-04:30\"."
  (multiple-value-bind
        (second minute hour
         day month year
         day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
     ;(decode-universal-time (get-universal-time) 450/100)
    (declare (ignore day-of-week dst-p))
    (multiple-value-bind (tz-h tz-m) (truncate (- tz))
      (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d~:[-~;+~]~2,'0d:~2,'0d"
              year month day
              hour minute second
              (plusp tz-h) (abs tz-h) (truncate (abs tz-m) 1/60)))))

(defun aux/append-strs-to-csv (strs csv)
  "Takes a list of strings, interpose with commas, prepend with a
newline character, and append to the given csv."
  (let ((s (apply #'concatenate 'string
                  (cons (make-string 1 :initial-element #\newline)
                        (aux/interpose ","
                                         (mapcar (lambda (str)
                                                   (concatenate 'string
                                                                "\"" str "\""))
                                                 strs))))))
    ;; TODO <mfiano> In this case, just use #'format. This code is overly complex .
    (with-open-file (f csv :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
      (write-sequence s f))))
