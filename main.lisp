(in-package :questionnaire)

;; Entry point
(defun execute-qnr (qnr &key (force nil))
  "If FORCE is nil, check if the questionnarie QNR has expired.
If so, ask the list of questions, and append the answer with a
time string to the path of the log file of QNR.

If FORCE is t, proceed forcefully."
  ;; 1. (execute-qnr mood-qnr)
  ;; 2. (mapcar 'execute-qnr '(mood-qnr weight-qnr))
  (if force
      (aux/append-strs-to-csv (ask-qnr qnr) (qnr-path qnr))
      (if (expiredp qnr)
          (aux/append-strs-to-csv (ask-qnr qnr) (qnr-path qnr))
          (print "Not expired yet. Do nothing."))))

;; Example usage:
;;   (execute-qnr example-qnr)
(setf example-qnr
      (make-qnr :name "Example Questionnaire"
                :path "/tmp/example.csv"
                :period (* 60 60 3)
                :qs '(("How are you?" ("Hype" "Great"
                                       "Meh" "Tired"
                                       "Down" "Sad" "Angry"))
                      ("Any comments?" ()))))
