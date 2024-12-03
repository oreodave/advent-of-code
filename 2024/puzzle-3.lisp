(load "util.lisp")

(defparameter input (uiop:read-file-string "3-input"))

(defun is-good-mul (str)
  (let* ((delimiter (search "," str))
         (end (search ")" str)))
    (if (or (null delimiter) (null end)
            (> delimiter end)
            (not (eq #\( (char str 3))))
        nil
        (let* ((first-arg (subseq str 4 delimiter))
               (second-arg (subseq str (1+ delimiter) end)))
          (and (every #'digit-char-p first-arg)
               (every #'digit-char-p second-arg))))))

(defun trim-mul (str)
  (subseq str 0 (1+ (search ")" str))))

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
         (mapcar (lambda (z1 z2) (cons z1 (subseq line z1 z2))) possible _)
         (remove-if-not (lambda (x) (is-good-mul (cdr x))) _)
         (mapcar (lambda (x) (cons (car x) (trim-mul (cdr x)))) _)
         (mapcar (lambda (x) (cons (car x) (parse-mul (cdr x)))) _))))

(format t "Round 1: ~a~%"
        (loop for (pos x y) in (parse-input-muls input)
              sum (* x y)))

(defun parse-input-conds (input)
  (let ((dos (search-all "do()" input))
        (donts (search-all "don't()" input)))
    (cons
     '(do . 0)
     (sort (append (mapcar (lambda (x) (cons 'do x)) dos)
                   (mapcar (lambda (x) (cons 'dont x)) donts))
           (lambda (x y) (< (cdr x) (cdr y)))))))

(defun current-cond (pos conds)
  (caar (last (remove-if (lambda (x) (> (cdr x) pos)) conds))))

(format t "Round 2: ~a~%"
        (let ((conds (parse-input-conds input))
              (muls (parse-input-muls input)))
          (loop for (pos x y) in muls
                for current = (current-cond pos conds)
                if (eq current 'do)
                  sum (* x y))))
