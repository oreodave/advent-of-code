(load "util.lisp")

(defpackage "aoc:1"
  (:use :cl "aoc-util"))

(in-package "aoc:1")

(fn round-1 (turns) (=> list fixnum)
  (loop with dial = 50
        with number-of-zeros = 0
        for (rotation . magnitude) in turns
        do (setf dial (mod (funcall rotation dial magnitude) 100))
        if (= dial 0)
          do (incf number-of-zeros)
        finally (return number-of-zeros)))

(fn round-2 (turns) (=> list fixnum)
  (loop with dial = 50
        with number-of-zeros = 0
        for (rotation . magnitude) in turns
        ;; FUCK I have to do this manually, too many edge cases
        do (loop for i from 1 to magnitude
                 for new-dial-value = (funcall rotation dial 1)
                 if (or (= new-dial-value 0) (= new-dial-value 100))
                   do (incf number-of-zeros)
                 do (setf dial (mod new-dial-value 100)))
        finally (return number-of-zeros)))

(let ((turns (loop for line in (uiop:read-file-lines "1-input")
                   for (rotation magnitude) = (->> line (split 1) multiple-value-list)
                   collect (cons (if (string= rotation "L")
                                     #'-
                                     #'+)
                                 (parse-integer* magnitude)))))
  (->> turns
       round-1
       (format t "Round 1: ~a~%"))
  (->> turns
       round-2
       (format t "Round 2: ~a~%")))
