(load "util.lisp")

(defpackage "aoc:6"
  (:use :cl "aoc-util"))

(in-package "aoc:6")

;; Computation, once we have everything parsed, is trivial...
(fn compute (operand-sets ops) (=> (list list) fixnum)
  (loop for operands in operand-sets
        for op in ops
        sum (apply op operands)))

;; What do you think this does?
(fn transpose (matrix) (=> list list)
  (loop for i from 1 to (length (car matrix))
        collect
        (loop
          for row in matrix
          collect (nth (1- i) row))))

(fn parse-op (op) (=> string function)
  (if (string= op "+") #'+ #'*))

(fn parse-input (filename) (=> string (values list list))
  ;; Returns (lines representing operands, parsed operators)
  (let* ((lines (uiop:read-file-lines filename))
         (last (car (last lines))))
    (values (->> lines length 1-
                 (subseq lines 0))
            (->> last
                 uiop:split-string
                 (remove-if ($>> (string= "")))
                 (mapcar #'parse-op)))))

;; The end of triviality
(fn parse-operand-sets-1 (operand-sets) (=> list list)
  (->>
   ;; Split every line in operand-sets by whitespace, deleting any trivial
   ;; strings
   (loop for op-set in operand-sets
         collect (->>
                  op-set
                  uiop:split-string
                  (remove-if ($>> (string= "")))))
   ;; transpose the operand set to get the right operands
   transpose
   ;; parse the integers contained in every op-set
   (mapcar ($>> (mapcar #'parse-integer)))))

(fn is-separator? (op-sets col) (=> (list fixnum) boolean)
  ;; Given a column, whitespace on every row => it's not a value
  (every (lambda (c) (char= c #\space))
     (loop for row in op-sets
           collect (nth col row))))

(fn parse-operand-sets-2 (operand-sets) (=> list list)
  ;; converts operand-sets into that weird cephalopod writing system

  ;; convert op-sets into a list of lists of chars
  (let ((op-sets (mapcar ($<> (coerce 'list)) operand-sets))
        columns)
    (loop
      with col-size = (length (car op-sets))
      with index = 0
      while (< index (col-size op-sets))

      ;; Skip any separators
      do
      (loop while (and (< index col-size)
                       (is-separator? op-sets index))
            do (incf index))

      ;; Extract a column till the next separator
      do
      (loop while (and (< index col-size)
                       (not (is-separator? op-sets index)))
            collect (loop for row in op-sets collect (nth index row)) into xs
            do (incf index)
            finally (setf columns (append columns (list xs)))))

    ;; Columns is now a set of groups of columns (by separator).  Each item in a
    ;; group is a set of characters.  Let's clean that up into groups of
    ;; integers.
    (-<> ($>>
          (mapcar ($>> (call-rev coerce 'string)
                  parse-integer*)))
         (mapcar columns))))

(multiple-value-bind (operand-sets ops) (parse-input "6-input")
  (->> operand-sets
       parse-operand-sets-1
       (call-rev compute ops)
       (format t "Round 1: ~a~%"))

  (->> operand-sets
       parse-operand-sets-2
       (call-rev compute ops)
       (format t "Round 2: ~a~%")))
