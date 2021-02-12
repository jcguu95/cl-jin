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
FORMOSA-BAKERY-HELPER>
(main :input "~/orders-formosa-bakery-1-23-overlandpark-weebly-com-1610776800-1611363412.csv")
"
[ellided string..]
```

In shell:

```
$ cat /tmp/test.txt
$ vim /tmp/test.txt -c ":\!paps < % > /tmp/out.pdf"
```

# Data flow

```
   /path/to/csv
-> columns
-> structure:order/meta+entries
-> string
```

# TODO

+ Use format instead of concatenate.

+ Add error-throwing for exceptions.

# Tested

Tested for [2021-01-15], [2021-01-22], [2021-02-04], [2021-02-12]
