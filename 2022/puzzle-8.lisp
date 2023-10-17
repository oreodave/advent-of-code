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
        (or
         (all lt-tree? row-left)
         (all lt-tree? row-right)
         (all lt-tree? col-top)
         (all lt-tree? col-bottom)))))

(defun how-many-visible ()
  (loop
    for x from 0 to (- n-trees 1)
    sum
    (reduce
     #'+
     (loop
       for y from 0 to (- n-trees 1)
       if (is-visible? x y)
         collect 1
       else
         collect 0))))

(format t "round 1: ~s~%" (how-many-visible))
