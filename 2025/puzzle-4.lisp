(load "util.lisp")

(defpackage "aoc:4"
  (:use :cl "aoc-util"))

(in-package "aoc:4")

(fn valid-cell-coord (graph x y) (=> (list fixnum fixnum) boolean)
  (and (>= x 0) (< x (length graph))
       (>= y 0) (< y (length (car graph)))))

(fn get-cell (graph x y) (=> (list fixnum fixnum) (or null character))
  (if (valid-cell-coord graph x y)
      (->> graph (nth x) (nth y))
      nil))

(fn adjacent-cell-coords (graph x y) (=> (list fixnum fixnum) cons)
  (loop
    for x_ from (1- x) to (1+ x)
    nconc
    (loop
      for y_ from (1- y) to (1+ y)
      for cell = (get-cell graph x_ y_)
      if cell
        collect (list x_ y_ cell))))

(fn get-rolls-with-adjacent-rolls (graph) (=> list cons)
  (loop
    for x from 0
    for row in graph
    nconc
    (loop
      for y from 0
      for cell in row
      if (char= cell #\@)
        collect
        (->> (adjacent-cell-coords graph x y)
             (remove-if-not ($>> (nth 2) (char= #\@)))
             length
             (list x y)))))


(fn get-good-rolls (graph) (=> list list)
  (->> graph
       get-rolls-with-adjacent-rolls
       (remove-if ($>> (nth 2) (< 4)))))

(fn round-1 (graph) (=> list fixnum)
  (->> graph
       get-good-rolls
       length))

(fn remove-cells (graph cells) (=> (list list) list)
  (loop for (x y) in cells
        do (setf (->> graph (nth x) (nth y)) #\.))
  graph)

(fn round-2 (graph) (=> list fixnum)
  (loop
    for good-rolls = (get-good-rolls graph)
    while (/= (length good-rolls) 0)
    sum (length good-rolls)
    do (->> good-rolls
            (mapcar ($<> (subseq 0 2)))
            (remove-cells graph)
            (setf graph))))

(let ((input (->> (uiop:read-file-lines "4-input")
                  (mapcar #'(lambda (x) (coerce x 'list))))))
  (->> input
       round-1
       (format t "Round 1: ~a~%"))
  (->> input
       round-2
       (format t "Round 2: ~a~%")))
