(load (merge-pathnames ".sbclrc" 
		       (user-homedir-pathname)))

(ql:quickload :curfew)
(in-package :curfew)

(loop (main) (sleep 5))
