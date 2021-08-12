(load (merge-pathnames ".sbclrc" 
                       (user-homedir-pathname)))

(ql:quickload :curfew)
(in-package :curfew)

(loop (ignore-errors (main))
      (sleep 4))
