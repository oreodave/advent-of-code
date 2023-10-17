(defun string-to-clist (str)
  (coerce str 'list))

(defun clist-to-string (clist)
  (if (atom clist)
      (string clist)
      (coerce clist 'string)))

(defun split-by-first (lst delim)
  "Splits LST by the first instance of DELIM"
  (let ((pos (position delim lst)))
    (if pos
        (cons (subseq lst 0 pos) (list (subseq lst (+ pos 1))))
        (error (format nil "No instance of ~a was found in ~a" delim lst)))))

(defun split-by-completely (lst delim)
  (if (or (null lst) (not (cdr lst)))
      (cons (list (car lst)) nil)
      (if (member delim lst)
          (destructuring-bind (first rest) (split-by-first lst delim)
            (cons first (split-by-completely rest delim)))
          (list lst))))

(defun get-lines (input-string)
  (with-input-from-string (s input-string)
    (loop for line = (read-line s nil)
          while line
          collect line)))
