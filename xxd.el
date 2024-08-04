#!/usr/bin/env emacs --script

(defun enumerate (l)
  (let ((index 0)
        (result nil))
    (dolist (element l (nreverse result))
      (push (list index element) result)
      (setq index (+ 16 index)))))

(defun hex-dump-line (address line)
  (concat
   (format "%08x" address)
   ": "
   (mapconcat (lambda (char) (format "%02X" char))
             (string-to-list line)
             " ")
   ;(if (> 1 (length line))
   ;    (elt l 0)
   ;  "nah")
   "  "
   line))

(defun xxd (str)
 (princ
  (concat
   (string-join
    (mapcar
     (lambda (args) (apply 'hex-dump-line args))
      (enumerate (split-string str "\n")))
   "\n")
  "\n")
 )
)

(defun slurp-stdin ()
  (condition-case nil (while t (insert (read-from-minibuffer "") "\n"))
    (error)))

(if (> (length argv) 0)
 (with-temp-buffer 
  (insert-file-contents (elt argv 0))  ; filename passed as CLI argument
  (xxd (buffer-string)))
 (with-temp-buffer
  (slurp-stdin)
  (xxd (buffer-string))))
