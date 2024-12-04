(load "util.lisp")

(defparameter input (uiop:read-file-string "3-input"))

(defun is-good-mul (str)
  (let ((start (search "(" str))
        (middle (search "," str))
        (end (search ")" str)))
    (and (not (null start)) (not (null middle)) (not (null end))
         ;; mul( <- 3 character
         (eq start 3)
         ;; Simple to understand
         (< start end)
         (< start middle)
         (< middle end)
         ;; Make sure the arguments are purely numbers
         (every #'digit-char-p (subseq str (1+ start) middle))
         (every #'digit-char-p (subseq str (1+ middle) end)))))

(defun parse-mul (str)
  (let ((start (search "(" str))
        (middle (search "," str))
        (end (search ")" str)))
    (list (parse-integer (subseq str (1+ start) middle))
          (parse-integer (subseq str (1+ middle) end)))))

(defun parse-input-muls (line)
  (let ((possible (search-all "mul" line)))
    (--> (cdr possible)
         (append _ (list (length line)))
         ;; index of mul -> (position substring)
         (mapcar (lambda (z1 z2) (cons z1 (subseq line z1 z2))) possible _)
         ;; remove any bad muls
         (remove-if-not (lambda (x) (is-good-mul (cdr x))) _)
         ;; parse muls
         (mapcar (lambda (x) (cons (car x) (parse-mul (cdr x)))) _))))

(format t "Round 1: ~a~%"
        (loop for (pos x y) in (parse-input-muls input)
              sum (* x y)))

(defun parse-input-conds (input)
  (let ((dos (search-all "do()" input))
        (donts (search-all "don't()" input)))
    (--> (append (mapcar (lambda (x) (cons 'do x)) dos)
                 (mapcar (lambda (x) (cons 'dont x)) donts))
         (sort _ (lambda (x y) (< (cdr x) (cdr y))))
         (cons '(do . 0) _))))

(defun current-cond (pos conds)
  (caar (last (remove-if (lambda (x) (> (cdr x) pos)) conds))))

(format t "Round 2: ~a~%"
        (let ((conds (parse-input-conds input))
              (muls (parse-input-muls input)))
          (loop for (pos x y) in muls
                for current = (current-cond pos conds)
                if (eq current 'do)
                  sum (* x y))))
