(in-package :dictionary)

;; readme
;; entry points
;; #'lookup-dict!
;; #'random-review!
;; #'review-history!

(defun lint (str)
  "Lint the marks in the input string STR."
  (arrows:-<> str
              (cl-ppcre:regex-replace-all (string #\Newline) <> " ")
              (cl-ppcre:regex-replace-all "-" <> " ")
              (cl-ppcre:regex-replace-all "\\+" <> " ")
              (cl-ppcre:regex-replace-all "\\(" <> " ")
              (cl-ppcre:regex-replace-all "\\)" <> " ")
              (cl-ppcre:regex-replace-all "\\." <> "")
              (cl-ppcre:regex-replace-all "\\?" <> "")
              (cl-ppcre:regex-replace-all "\"" <> "")
              (cl-ppcre:regex-replace-all "," <> "")
              (cl-ppcre:regex-replace-all "!" <> "")
              (cl-ppcre:regex-replace-all ":" <> "")
              (cl-ppcre:regex-replace-all ";" <> "")))

(defun sentence->words (sentence)
  "Break the input SENTENCE into a list of words."
  (remove-duplicates
   (split-sequence:split-sequence #\Space
    (string-downcase (lint sentence)))
   :test #'string-equal))

(defun sentence->dmenu (sentence)
  "Break the input SENTENCE into a list of words. Let the user
choose a word by using dmenu. Return the selected word."
  ;; Usage: set sentence to be (trivial-clipboard:text)
  (uiop:run-program
   (format nil "echo -e \"~a\" | dmenu"
           (format nil "~{~a\\n~}"
                   (sentence->words sentence)))
   :output '(:string :stripped t)))

(defvar *dict-dir*
  (concatenate 'string
               (sb-unix::posix-getenv "HOME")
               "/data/storage/dictionary"))

(defun lookup-word (word)
  "Look up the input word WORD by using sdcv. Format the output
as a string."
  ;; This relies on the personal dictionary collection in *DICT-DIR*.
  (uiop:run-program
   (format nil
           "sdcv --data-dir=\"~a\" -n ~a | sed 's/^[ \t\n ]*//g'"
           *dict-dir* word)
   :output '(:string :stripped t)))

(defun notify (title content &key (expire-timeout 0))
  "Send notification using dmenu. EXPIRE-TIMEOUT is an INT32 in
milisecond; 0 means infinite while (-1) means default."
  ;; TODO Put this to another personal package.
  ;; taken from https://github.com/death/dbus/blob/master/examples/notify.lisp
  ;;
  (dbus:with-open-bus (bus (dbus:session-server-addresses))
    (dbus:with-introspected-object
        (notifications bus "/org/freedesktop/Notifications"
                       "org.freedesktop.Notifications")
      (notifications "org.freedesktop.Notifications" "Notify"
                     "Test" 0 "" title content '() '() expire-timeout))))

(defun lookup-dict-string (string &key force)
  "Let the user pick a word in the input STRING using dmenu.
Lookup the selected word from the dictionary. Announce the result
using notify-send."
  (let* ((word (if force
                   string
                   (sentence->dmenu string))))

    ;; lookup WORD and push to notification
   ;(notify "word.lisp" (lookup-word word))
    (jin-utils:notify-send "word.lisp" (lookup-word word))

    ;; if selected WORD isn't in the STRING, strip the STRING
    (unless (member word (sentence->words string)
                    :test #'string-equal)
      (setf string ""))

    ;; wrap entry with time and context; render as a string.
    (let ((time (local-time:now))
          (context ""))
      (format nil "(\"~a\"~% \"~a\"~% ~s~% ~s)~%~%"
              time word string context))))

(defun lookup-dict! ()
  "Let the user pick a word from the string in the clipboard
using dmenu. Lookup the selected word's definition in the
dictionary. Announce the result by notify-send. And save the
result to a clip file."
  (let* ((clip (trivial-clipboard:text))
         (result (lookup-dict-string clip)))

    ;; write result to file
    (with-open-file (file (pathname "~/.nb/clip.txt")
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
      (write-sequence result file))))

(defun read-clip ()
  (setf *clip*
        (eval (read-from-string
               (format nil "'(~a)" (uiop:read-file-string "~/.nb/clip.txt"))))))

(defvar *review-hist* nil)
(defvar *review-hist-length* 10)
(defun random-review (&key lookup)
  (let* ((entry (nth (random (length *clip*)) *clip*))
         (word (nth 1 entry))
         (sentence (nth 2 entry))
         (context (nth 3 entry)))

    ;; add word to history and truncate if needed
    (progn
      (push word *review-hist*)
      (setf *review-hist* (subseq *review-hist*
                                  0 (min *review-hist-length* (length *review-hist*)))))

    ;; if LOOKUP is set T, lookup the string without word selection.
    (when lookup (lookup-dict-string word :force t))

    ;; format the final result as a string.
    (format nil "~a~%~%~a~%~%[context]~%~a" word sentence context)))

(defun random-review! ()
  (read-clip)
 ;(notify (format nil "Review the word!~%") (random-review)))
  (jin-utils:notify-send (format nil "Review the word!~%") (random-review)))

(defun review-history! ()
 ;(notify "Review History" (format nil "~s" *review-hist*)))
  (jin-utils:notify-send "Review History" (format nil "~s" *review-hist*)))
