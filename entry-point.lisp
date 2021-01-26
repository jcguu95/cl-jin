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
          (format t "~a not expired yet. Do nothing.~%"
                  (qnr-name qnr)))))

(defun check-and-notify (qnr)
  (if (expiredp qnr)
      (progn
        (write-line
         (concatenate 'string
                      (qnr-name qnr)
                      ": EXPIRED. Sending notification."))
        (asdf:run-shell-command
         (concatenate 'string
                      "notify-send \""
                      (qnr-name qnr)
                      ": EXPIRED!\"")))
      (write-line
       (concatenate 'string
                    (qnr-name qnr)
                    ": NOT expired yet."))))
