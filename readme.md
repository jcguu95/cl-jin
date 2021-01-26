See entry point in `main.lisp`.

# TODO

+ Make it an executable.
+ Add format checkers. Need to upgrade my regex-fu for this.
+ Handle double quoting in csv file.. cl-csv will break
  ;; Note that the fields in the csv file should not contain double
  ;; quotation mark. E.g.
  ;; 1,2,3,""     => fine
  ;; 1,2,3,""hi"" => not fine
  csv breaks too easily.. what's another kind of db that's transparent?
+ Handle error when csv file doesn't exist.
+ Add auto doctor.

# Configuration

Add the following in `config.lisp`!

``` common-lisp
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
```
