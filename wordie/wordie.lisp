(in-package :jin.wordie)

;; Check db health by evaluating (%review).

(defparameter *last-context* nil)
(defparameter *review-hist* nil)
(defparameter *review-hist-length* 10)
(defparameter *dict-dir*
  (concatenate 'string
               (sb-unix::posix-getenv "HOME")
               "/data/storage/dictionary"))
(defparameter *clip-dir* "~/.nb/clip.el")

(defun lint (str)
  "Lint the marks in the input string STR."
  (cl-ppcre:regex-replace-all "[-\\+\\(\\)\\n\\.\\?\"!:;,]" str " "))

(defun words<-sentence (sentence)
  "Break the input SENTENCE into a list of words."
  (remove-duplicates
   (split-sequence:split-sequence #\Space
    (string-downcase (lint sentence)))
   :test #'string-equal))

(defun get-context ()
  "Push the current window info to the global variable
*LAST-CONTEXT*, before the current window info is unaccessible
after calling dmenu."
  (let ((context (uiop:run-program
                  ;; returns the current window name
                  "DISPLAY=:0 && xdotool getwindowfocus getwindowname"
                  :output '(:string :stripped t))))
    (setf *last-context* context)))

(defun select<-sentence (sentence)
  "Break the input SENTENCE into a list of words. Let the user
choose a word by using jin.utils:select. Return the selected
word."
  (get-context)
  (jin.utils:select (words<-sentence sentence)))

(defun lookup-word (word)
  "Look up the input word WORD by using sdcv. Format the output
as a string."
  ;; This relies on the personal dictionary collection in *DICT-DIR*.
  (uiop:run-program
   (format nil
           "sdcv --data-dir=\"~a\" -n ~a | sed 's/^[ \t\n ]*//g'"
           *dict-dir* word)
   :output '(:string :stripped t)))

(defun lookup-dict-string (string &key force)
  "Let the user pick a word in the input STRING using jin.utils:select.
Lookup the selected word from the dictionary. Announce the result
using notify-send. Return the string to be written to file
later."
  (let* ((word (if force
                   string
                   (select<-sentence string))))

    ;; Lookup WORD and push to notification.
    ;;
    ;; The old way is to use notify-send. The new way is to use
    ;; lexic-search-dropdown. FIXME When emacsclient is absent
    ;; use the old way then.
    ;; (jin.utils:notify-send "word.lisp" (lookup-word word))
    (jin.utils:lexic-search-dropdown word)

    ;; if selected WORD isn't in the STRING, strip the STRING
    (unless (member word (words<-sentence string)
                    :test #'string-equal)
      (setf string ""))

    ;; wrap entry with time and context; render as a string.
    (let ((time (local-time:now))
          (comment nil
            ;; (jin.utils:rofi-or-dmenu () "Comment: ")
            ;; (uiop:run-program
            ;;  (format nil "echo \"\" | dmenu -p \"Comment: \" ")
            ;;  :output '(:string :stripped t))
            ))
      (format nil "(\"~a\"~% \"~a\"~% ~s~% ~s ~% ~s)~%~%"
              time word string *last-context* comment))))

(defun lookup-dict-from-clip! ()
  "Let the user pick a word from the string in the clipboard
using dmenu. Lookup the selected word's definition in the
dictionary. Announce the result by notify-send. And save the
result to a clip file."
  (let* ((clip (trivial-clipboard:text))
         (result (lookup-dict-string clip)))
    ;; write result to file
    (with-open-file (file (pathname *clip-dir*)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
      (write-sequence result file))))

(defun load-clip ()
  (setf *clip*
        (eval
         (read-from-string
          (format nil "'(~a)" (uiop:read-file-string *clip-dir*))))))

(defun select-review! ()
  "TODO"
  (load-clip)
  (jin.utils:notify-send
   (format nil "Review the word!~%")
   (%review :word (jin.utils:rofi-or-dmenu
                   (reverse (mapcar #'cadr *clip*))
                   "Select to review: "))))

(defun %review (&key word lookup)
  (load-clip)
  (let* ((entry (if word
                    (find-if (lambda (x)
                               (equal word (nth 1 x)))
                             *clip*)
                    ;; word nil => random select
                    (nth (random (length *clip*)) *clip*)))
         (word (nth 1 entry))
         (sentence (nth 2 entry))
         (context (nth 3 entry)))
    ;; add word to history and truncate if needed
    (progn
      (push word *review-hist*)
      (setf *review-hist* (subseq *review-hist* 0
                                  (min *review-hist-length*
                                       (length *review-hist*)))))
    ;; if LOOKUP is set T, lookup the string without word selection.
    (when lookup (lookup-dict-string word :force t))
    ;; format the final result as a string.
    (format nil "~a~%~%~a~%~%[context]~%~a" word sentence context)))

(defun review! (word)
  (load-clip)
  (jin.utils:notify-send
   (format nil "Review the word!~%")
   (%review :word word)))

(defun review-history! ()
  (jin.utils:notify-send
   "Review History"
   (format nil "~s" *review-hist*)))
