(load "util.lisp")

(defpackage "aoc:3"
  (:use :cl "aoc-util"))

(in-package "aoc:3")

(fn compose-joltage (x y) (=> (integer integer) integer)
  (->> x (* 10) (+ y)))

(fn maximum (xs) (=> list cons)
  (--> _
    (loop for x in xs maximizing x)
    (cons _ (position _ xs))))

(fn best-joltage-1 (bank) (=> list cons)
  (destructuring-bind (max-val . max-pos) (maximum bank)
    (if (->> bank length 1- (= max-pos))
        ;; best value at end => next best is the first digit
        (cons (->> bank length 1-
                   (subseq bank 0)
                   maximum car)
              max-val)
        ;; best value not at end => next best is the second digit
        (->> max-pos 1+
             (subseq bank)
             maximum car
             (cons max-val)))))

(fn round-1 (banks) (=> list fixnum)
  (loop for bank in banks
        for (first-digit . second-digit) = (best-joltage-1 bank)
        sum (compose-joltage first-digit second-digit)))

(fn best-joltage-2 (bank) (=> list fixnum)
  #| Sliding window greedy search?

  We look at a sequence of digits in bank, choose the best one, then move onto
  the next window.  We need the windows, at all times, to have enough digits for
  us to pick a good one from.  In this case we need to choose 12, so at any one
  time we need to be examining at most 12 digits (decrementing as we get more
  digits).  The next window needs to be _after_ the position of the best value
  we picked in our current window.  |#
  (loop
    with window-start = 0
    for n from 12 downto 1
    for window = (subseq bank window-start (-<> bank length 1+ (- n)))
    for (max-val . max-pos) = (maximum window)
    do (setf window-start (+ max-pos 1 window-start))
    collect max-val into digits
    finally (return (reduce #'compose-joltage digits))))

(fn round-2 (banks) (=> list integer)
  (->> banks (mapcar #'best-joltage-2) (reduce #'+)))

(let ((input (loop for line in (uiop:read-file-lines "3-input")
                   for chars = (coerce line 'list)
                   for digit-strings = (mapcar #'string chars)
                   collect (mapcar #'parse-integer* digit-strings))))
  (->> input
       round-1
       (format t "Round 1: ~a~%"))
  (->> input
       round-2
       (format t "Round 2: ~a~%")))
