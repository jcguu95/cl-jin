(in-package :jin.utils)

(defun select (strings &key (timeout 10))
  (if (ping-emacsclient)
      (call-emacs-dropdown-ivy-select strings)
      (dmenu strings)))

(defun dropdown-ivy-select-sexpr
    (&key (ivy-height 50) (name "[TOP] ivy-read") (alpha '(80 70)) (unsplittable t)
       (top 10) (left 210) (width 80) (height 12) (prompt "Select: "))
  "Generate the elisp code to call a dropdown window for
selection in ivy."
  `(lambda (strings)
     "Call #'ivy-read in a dropdown frame, wait for user
selection, and return the selected string."
     (let ((strings (if strings strings (list "")))
           (result)
           (ivy-height ,ivy-height)
           (new-frame (make-frame
                       '((window-system . x)
                         (name . ,name)
                         (alpha . ,alpha) (unsplittable . ,unsplittable)
                         (top . ,top) (left . ,left)
                         (width . ,width) (height . ,height)))))
       (select-frame new-frame)
       (select-frame-set-input-focus new-frame)
       (delete-other-windows)
       (hide-mode-line-mode)
       (unwind-protect
            (setf result (ivy-read ,prompt strings :sort nil))
         (delete-frame)
         result))))

(defun call-emacs-dropdown-ivy-select
    (strings &key (timeout 10)
               (ivy-height 50) (name "[TOP] ivy-read") (alpha '(80 70)) (unsplittable t)
                (top 10) (left 210) (width 80) (height 12) (prompt "Select: "))
  "Call `emacsclient' to call #'ivy-read in a dropdown frame, wait
for user selection, and return the selected string."
  (eval-in-emacs
   `(funcall
     ,(dropdown-ivy-select-sexpr :ivy-height   ivy-height
                                 :name         name
                                 :alpha        alpha
                                 :unsplittable unsplittable
                                 :top          top
                                 :left         left
                                 :width        width
                                 :height       height
                                 :prompt       prompt)
     (list ,@strings))
   :timeout timeout))

(defun dmenu (strings &optional (prompt ""))
  "Receive a list of STRINGs with a optional PROMPT.
Let the user select a string using rofi or dmenu, and return the
selected string."
  (string-right-trim
   '(#\Newline)
   (with-output-to-string (out)
     (with-input-from-string
         (in (format nil "~{~a~%~}" strings))
       (let ((rofi (whereis "rofi"))
             (dmenu (whereis "dmenu"))
             command args)
         (if rofi
             (progn (setf command rofi)
                    (setf args (list "-dmenu" "-p" prompt)))
             (progn (setf command dmenu)
                    (setf args (list "-p" prompt "-fn"
                                     (format nil "~a-~a" font size)))))
         (sb-ext:run-program command args
                             :input in
                             :output out
                             :error *standard-output*))))))
