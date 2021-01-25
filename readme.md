# Usage

In `sbcl` repl:

```
CL-USER> (ql:quickload :formosa-bakery-helper)
To load "formosa-bakery-helper":
  Load 1 ASDF system:
    formosa-bakery-helper
; Loading "formosa-bakery-helper"
...
(:FORMOSA-BAKERY-HELPER)
CL-USER> (in-package :formosa-bakery-helper)
#<PACKAGE "FORMOSA-BAKERY-HELPER">
FORMOSA-BAKERY-HELPER> (main :input "~/orders-formosa-bakery-1-23-overlandpark-weebly-com-1610776800-1611363412.csv")
"
[ellided string..]
```

In shell:

```
$ cat /tmp/test.txt
$ vim /tmp/test.txt -c ":\!paps < % > /tmp/out.pdf"
```

# Data Flow

/path/to/csv -> columns -> structure:order/meta+entries -> string

# TODO

+ Disable slynk's string elision by default

  ``` common-lisp
  (setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)
  ```

+ Use format instead of concatenate.

+ Add error-throwing for exceptions.
