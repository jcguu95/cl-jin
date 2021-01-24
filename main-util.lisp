(in-package :questionnaire)

(defstruct qnr
  name path period qs)

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
                                      (aux/interpose "\\n" default-answers)))
                         :output :stream))
                       :output :string))))

(defun ask-qnr (qnr)
  "Run through the list of questions in the questionnarie QNR and
construct a list of answers. Prepend it by the currrent time (as
a string). Return the list"
  (cons (aux/format-time-string-now)
        (mapcar #'ask-q (qnr-qs qnr))))
