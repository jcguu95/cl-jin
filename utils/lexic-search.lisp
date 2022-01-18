(in-package :jin.utils)

(defun lexic-search- (word)
  "Call `emacsclient' to search the WORD with lexic.el."
  (eval-in-emacs
   `(funcall
     (lambda (word)
       "Go to the lexic buffer of the WORD."
       (if (and (stringp word)
                (not (zerop (length word))))
           (with-current-buffer (get-buffer-create lexic-buffer-name)
             (setq buffer-read-only nil)
             (erase-buffer)
             (insert (lexic-search word)))
           (message "WORD must be a nonempty string.")))
     ,word)))

(defun lexic-dropdown- ()
  "Call `emacsclient' to make a dropdown frame with lexic buffer
opened."
  (eval-in-emacs
   `(let* ((result)
           (ivy-height 50)
           (new-frame (make-frame '(;; (user-position . t) (minibuffer . only)
                                    ;; ; minibuffer option seems buggy.
                                    (window-system . x)
                                    (name . "[TOP] lexic-buffer-view") (alpha . (80 70))
                                    (top . 10) (left . 210) (width . 80) (height . 12)))))
      (select-frame new-frame)
      (select-frame-set-input-focus new-frame)
      (jin/goto-lexic-buffer)
      (delete-other-windows)
      (hide-mode-line-mode))))

(defun lexic-search-dropdown (word)
  "Search the WORD with lexic.el, and present the result in a
dropdown lexic frame. Prerequisites: emacsclient, lexic.el."
  (lexic-search- word)
  (lexic-dropdown-))
