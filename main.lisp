(ql:quickload "cl-date-time-parser")
(ql:quickload "cl-csv")
;; Note that the fields in the csv file should not contain double
;; quotation mark. E.g.
;; 1,2,3,""     => fine
;; 1,2,3,""hi"" => not fine

(ql:quickload "alexandria")
(defun adhoc/interpose (sep lst)
  ;; an adhoc list operation
  "For example, (adhoc/interpose 7 '(1 2 3)) returns (1 7 2 7 3)."
  (cdr (alexandria:flatten
    (mapcar (lambda (e) (cons sep e)) lst))))



(defun adhoc/format-time-string-now ()
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

(defstruct qnr
  name path period qs)

(setf mood-qnr
      (make-qnr :name "Mood Questionnaire"
                :path "~/data/storage/body/moodlogger.csv"
                :period (* 60 60 3)
                :qs '(("How are you?" ("Hype" "Great"
                                       "Meh" "Tired"
                                       "Down" "Sad" "Angry"))
                      ("Any comments?" ()))))

(setf weight-qnr
      (make-qnr :name "Weight Questionnarie"
                :path "~/data/storage/body/weightlogger.csv"
                :period (* 60 60 24)
                :qs '(("Enter current weight." ())
                      ("Any comments?" ()))))

(defun execute-qnr (qnr &key (force-p nil))
  "If FORCE-P is nil, check if the questionnarie QNR has expired.
If so, ask the list of questions, and append the answer with a
time string to the path of the log file of QNR.

If FORCE-P is t, proceed forcefully."
  ;; 1. (execute-qnr mood-qnr)
  ;; 2. (mapcar 'execute-qnr '(mood-qnr weight-qnr))
  (if force-p
      (append-strs-to-csv (ask-qnr qnr) (qnr-path qnr))
      (if (expiredp qnr)
          (append-strs-to-csv (ask-qnr qnr) (qnr-path qnr))
          (print "Not expired yet. Do nothing."))))

(defun expiredp (qnr)
  ;; Each qnr has its own period.
  ;; The period judges if the qnr has expired.
  ;; Returns t if it has expired; orelse nil.
  (let* ((filepath  (pathname (qnr-path qnr)))
         (time-diff (- (get-universal-time)
                       (last-update filepath)))
         (period    (qnr-period qnr)))
    (> time-diff period)))

(defun last-update (pathname)
  "Expect the pathname of a csv file. Read its zeroth field of
the last row, and expect it to be a time string in the form, for
example, as that of \"2020-12-31 06:07:05-06:00\". Output the
universal time (in second)."
  ;; TODO add format checker
  ;; expect raw-last-update as a time string in the form
  ;; e.g. 2020-12-17 06:27:05-06:00
  ;; (time zone is crutial)
  (let* ((content (cl-csv:read-csv pathname))
         (raw-last-update (car (car (last content)))))
    (cl-date-time-parser:parse-date-time raw-last-update)))

(defun ask-q (question)
  "Input a question, output the answer as a string with the last
newline trimmed."
; A question is a list like this
;   '("How are you?"
;     ("Hype" "Great" "Meh" "Tired" "Down" "Sad" "Angry"))
; TODO Check that the answer does not contain quotation marks.
; TODO Add a format checker. E.g. "%d%d:%d%d". .. Alas, my regex-fu :-(
  (let ((prompt (car question))
        (default-answers (car (cdr question))))
    (string-right-trim
     '(#\Newline)
     (uiop:run-program (list "dmenu" "-p" prompt)
                       :input
                       (uiop:process-info-output
                        (uiop:launch-program
                         (list "echo" "-e"
                               (apply #'concatenate 'string
                                      (adhoc/interpose "\\n" default-answers)))
                         :output :stream))
                       :output :string))))

(defun ask-qnr (qnr)
  "Run through the list of questions in the questionnarie QNR and
construct a list of answers. Prepend it by the currrent time (as
a string). Return the list"
  (cons (adhoc/format-time-string-now)
        (mapcar #'ask-q (qnr-qs qnr))))

(defun append-strs-to-csv (strs csv)
  "Takes a list of strings, interpose with commas, prepend with a
newline character, and append to the given csv."
  (let ((s (apply #'concatenate 'string
                  (cons (make-string 1 :initial-element #\newline)
                        (adhoc/interpose ","
                                         (mapcar (lambda (str)
                                                   (concatenate 'string
                                                                "\"" str "\""))
                                                 strs))))))
    ;; TODO <mfiano> In this case, just use #'format. This code is overly complex .
    (with-open-file (f csv :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
      (write-sequence s f))))
