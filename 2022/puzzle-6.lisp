(defvar input (with-input-from-string (s (uiop:read-file-string "2022/6-input"))
                (read-line s nil)))

(defun is-unique (lst)
  (or (null lst)
     (and (not (member (car lst) (cdr lst)))
        (is-unique (cdr lst)))))

(defun string-to-list (str)
  (loop for char across str collect char))

(defun first-round (input)
  (defun calculate-position (nth)
    "Calculate the actual position of the nth item."
    (if nth
        (+ nth 4)
        nil))

  (let* ((char-list (string-to-list input))
         (unique-check (mapcar (lambda (x y z w) (is-unique (list x y z w))) char-list (cdr char-list) (cdr (cdr char-list)) (cdr (cdr (cdr char-list))))))
    (calculate-position (position t unique-check))))

(format t "First round: ~a~%" (first-round input))

(defun successive-by (lst n)
  "Returns consecutive subsequences of size N in LST."
  (loop
    for item in lst
    for i from 0
    collect
    (if (> (+ n i) (length lst))
        nil
        (subseq lst i (+ n i)))))

(defun second-round (input)
  (defun calculate-position (nth)
    "Calculate the actual position of the nth item."
    (if nth
        (+ nth 14)
        nil))


  (let* ((char-list (string-to-list input))
         (unique-check (mapcar #'is-unique (successive-by char-list 14))))
    (calculate-position (position t unique-check))))

(format t "Second round: ~a~%" (second-round input))
