#!/usr/bin/env emacs --script

(defun partition-string-by-k (str &optional k)
  (let ((k (or k 2)))	; k default: 2
    (if (string-empty-p str)
        nil
      (let* ((len (min k (length str)))
             (group (substring str 0 len))
             (rest (substring str len)))
        (cons group
              (partition-string-by-k rest k))))))

(defun ascii-decimal-pairs-to-ascii-hex-pairs (decimal-pairs)
  (mapconcat
   (lambda (pair)
     (apply #'format
	    (if (eql 2 (length pair))
	      "%02x%02x"
	      "%02x")  ; for dangling non-pairs: ((31 32) (33))
	    pair))
   decimal-pairs " "))

(defun string-pairs-to-ascii-decimal-pairs (string-pairs)
  (mapcar
   (lambda (pair)
     (string-to-list
      pair))
   string-pairs))

(defun hex-dump-line (address line)
  (concat
   (format "%08x" address)
   ": "
   (format "%-39s"  ; TODO: support alternate widths (-c/--col 16)
    (ascii-decimal-pairs-to-ascii-hex-pairs
    (string-pairs-to-ascii-decimal-pairs
     (partition-string-by-k line 2))))
   "  "
   (replace-regexp-in-string "[^\x20-\x7E]" "." line)))

(defun enumerate (l &optional _start _step)
  (let ((index (or _start 0))  ; _start default: 0
        (result nil))
    (dolist (element l (nreverse result))
      (push (list index element) result)
      (setq index (+ (or _step 1) index)))))  ; _step default: 1

(defun xxd (str)
 (princ
  (concat
   (string-join
    (mapcar
     (lambda (args) (apply 'hex-dump-line args))
      (enumerate (partition-string-by-k str 16) 0 16))
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
