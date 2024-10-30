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
        (list (subseq lst 0 pos) (subseq lst (+ pos 1)))
        (error (format nil "No instance of ~a was found in ~a" delim lst)))))

(defun split-by-completely (lst delim)
  (cond
    ((or (null lst) (not (cdr lst)))
     (list (car lst)))
    ((not (member delim lst))
     (list lst))
    (t
     (loop
       for (start rest) = (split-by-first lst delim)
         then (split-by-first rest delim)
       collect start
       if (not (member delim rest))
         collect rest
         and do (loop-finish)))))

(defun get-lines (input-string)
  (with-input-from-string (s input-string)
    (loop for line = (read-line s nil)
          while line
          collect line)))

(defun remove-nth (n lst)
  (loop for el in lst
        for i from 0
        if (not (= i n))
          collect el))

(defmacro --> (first &rest functions)
  (let ((builder first))
    (loop for function in functions
          do (setq builder `(let ((it ,builder))
                              ,function)))
    builder))
