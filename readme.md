# Flow

/path/to/csv -> columns -> structure:order/meta+entries -> string

# TODO

+ Disable slynk's string elision by default

  ``` common-lisp
  (setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)
  ```

+ Use format instead of concatenate.

+ Add error-throwing for exceptions.
