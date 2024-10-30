(load "lib")
(defparameter input (uiop:read-file-string "8-input"))
(defparameter lines (get-lines input))
;; Just converts every line into a sequence of integral digits
(defparameter trees
  (mapcar (lambda (line) (mapcar (lambda (char) (parse-integer (string char))) (string-to-clist line)))
     lines))
;; Size of a row or column
(defparameter n-trees (length trees))

(defun get-column (x)
  (loop for row in trees
        collect (nth x row)))

(defun get-row (y)
  (nth y trees))

(defun get-tree (x y)
  (nth x (nth y trees)))

(defun at-end? (a)
  (or (= a 0) (= a (- n-trees))))

(defun is-visible? (x y)
  (if (or (= x 0) (= x (- n-trees 1))
          (= y 0) (= y (- n-trees 1)))
      t
      (let* ((tree (get-tree x y))
             (lt-tree? (lambda (a) (< a tree)))
             (row (get-row y))
             (row-left (subseq row 0 x))
             (row-right (subseq row (+ x 1)))
             (col (get-column x))
             (col-top (subseq col 0 y))
             (col-bottom (subseq col (+ y 1))))
        (some #'identity
           (mapcar (lambda (x) (every lt-tree? x))
              (list row-left row-right col-top col-bottom))))))

(defun how-many-visible ()
  (loop
    for x from 0 below n-trees
    sum
    (reduce
     #'+
     (loop
       for y from 0 below n-trees
       if (is-visible? x y)
         collect 1
       else
         collect 0))))

(format t "round 1: ~s~%" (how-many-visible))

(defun keep-till-last-satisfying (pred lst &optional acc)
  "Iteratively collect all items of LST that satisfy PRED, including the
first member which does not satisfy PRED."
  (cond
    ((null lst) acc)
    ((not (funcall pred (car lst)))
     (cons (car lst) acc))
    (t
     (keep-till-last-satisfying pred (cdr lst) (cons (car lst) acc)))))

(defun scenic-score (x y)
  "Calculate the scenic score of the tree at position X, Y."
  (let* ((tree (get-tree x y))
         (lt-tree? (lambda (a) (< a tree)))
         (row (get-row y))
         (row-left (reverse (subseq row 0 x)))
         (row-right (subseq row (+ x 1)))
         (col (get-column x))
         (col-top (reverse (subseq col 0 y)))
         (col-bottom (subseq col (+ y 1))))
    (reduce #'*
            (mapcar (lambda (x) (length (keep-till-last-satisfying lt-tree? x)))
               (list row-left row-right col-top col-bottom)))))

(format t "round 2: ~s~%"
        (car (sort
              ;; Calculate all scenic scores
              (loop for x from 0 below n-trees
                    nconcing
                    (loop for y from 0 below n-trees
                          collect
                          (scenic-score x y)))
              #'>)))
