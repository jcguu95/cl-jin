(in-package :questionnaire)

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
