It's working!

# TODO

+ Add format checkers. Need to upgrade my regex-fu for this.
+ Make it an executable.
+ Handle double quoting in csv file.. cl-csv will break
  ;; Note that the fields in the csv file should not contain double
  ;; quotation mark. E.g.
  ;; 1,2,3,""     => fine
  ;; 1,2,3,""hi"" => not fine
