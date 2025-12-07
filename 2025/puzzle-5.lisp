(load "util.lisp")

(defpackage "aoc:5"
  (:use :cl "aoc-util"))

(in-package "aoc:5")

(fn parse-input (input) (=> list (values list list))
  (let ((input (-<> (position "" input :test #'string=)
                    (split input)
                    multiple-value-list)))
    (values
     (loop for range in (car input)
           for bounds = (uiop:split-string range :separator '(#\-))
           collect (mapcar #'parse-integer* bounds))
     (mapcar #'parse-integer* (cdadr input)))))

(fn in-range (n range) (=> (integer list) boolean)
  (destructuring-bind (lower upper) range
    (and (<= n upper) (>= n lower))))

(fn round-1 (ranges items) (=> (list list) fixnum)
  (loop
    for item in items
    sum
    (loop
      for range in ranges
      if (in-range item range)
        return 1
      finally (return 0))))

(fn round-2 (ranges) (=> list integer)
  (loop
    with ranges = (sort ranges #'(lambda (x y) (< (car x) (car y))))
    with end = 0
    for (lower upper) in ranges
    if (> lower end)
      sum (1+ (- upper lower))          ;add the size of the range to our running total
      and do (setf end upper)           ;make the end of our current range the upper bound
    else
      ;; (lower, upper) contained in ranges => remove the intersect
      sum (- (max upper end) end)
      and do (setf end (max upper end))))

(let ((input (uiop:read-file-lines "5-input")))
  (multiple-value-bind (ranges items) (parse-input input)
    (->> (round-1 ranges items)
         (format t "Round 1: ~a~%"))
    (->> (round-2 ranges)
         (format t "Round 2: ~a~%"))))
