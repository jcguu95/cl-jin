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
