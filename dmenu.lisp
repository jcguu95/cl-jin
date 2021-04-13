(in-package :jin-utils)

(defvar *output-file* "/tmp/jin-dmenu.lisp.log")

(defun dmenu (options &optional prompt &key force-from-options)
  "Example usage:

     (dmenu '(\"coffee\" \"tea\") \"Coffee or tea?\")

If FORCE-FROM-OPTIONS is NON-NIL, then the selected result must
be a member of the given options. Otherwise, fire an error."

  ;; Write to error output file, just to make sure it exists
  ;; later. This is a dirty fix for that uiop:run-program does
  ;; not seem to have a handler for :error-output-does-not-exist.
  (with-open-file (str *output-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "jin-dmenu.lisp ran at universal time ~a~%"
            (get-universal-time)))

  (let ((result (uiop:run-program
                 (format nil "echo -e \"~{~a\\n~}\" | dmenu -p \"~a\""
                         options prompt)
                 :output '(:string :stripped t)
                 :error-output *output-file*
                 :if-error-output-exists :append)))

    (if force-from-options
        ;; If FORCE-FROM-OPTIONS is non-nil, then throw an error
        ;;   if the result is not a member of OPTIONS.
        (if (member result options :test #'string=)
            result
            (error "Output isn't among options."))
        result)))

;; (dmenu '("coffee" "tea") "Coffee or tea?")
;; (dmenu '("coffee" "tea") "Coffee or tea?" :force-from-options t)
