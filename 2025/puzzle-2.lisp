(load "util.lisp")

(defpackage "aoc:2"
  (:use :cl "aoc-util"))

(in-package "aoc:2")

(fn parse-input (filename) (=> string list)
  (loop for item in (-<> (uiop:read-file-string filename)
                         (uiop:split-string :separator '(#\,)))
        for split-range = (uiop:split-string item :separator '(#\-))
        collect (mapcar ($>> (string-trim '(#\space)) parse-integer*) split-range)))

(fn invalid-id-1 (n) (=> fixnum boolean)
  (let ((str (format nil "~a" n)))
    (if (= 1 (mod (length str) 2))
        nil
        (let ((items (multiple-value-list (split (/ (length str) 2) str))))
          (string= (car items) (cadr items))))))

(fn round-1 (id-ranges) (=> list fixnum)
  (loop with invalid-ids = 0
        for (lower upper) in id-ranges
        do (loop for i from lower to upper
                 if (invalid-id-1 i)
                   do (incf invalid-ids i))
        finally (return invalid-ids)))

(fn invalid-id-2 (n) (=> fixnum boolean)
  (loop
    ;; Loop setup
    with str = (format nil "~a" n)
    with window-len = 1
    with window-str = (subseq str 0 1)
    with i = 1

    while (< i (length str))
    for chunk = (subseq str i (min (length str) (+ i window-len)))
    if (< (length chunk) window-len)
      return nil
    if (string= chunk window-str)
      ;; check the next chunk
      do (incf i window-len)
    else
      ;; we need to increase the size of our window
      do (setf window-len (1+ i)
               window-str (subseq str 0 window-len)
               i (1+ i))
    finally (return (->> str length (= window-len) not))))

(fn round-2 (id-ranges) (=> list fixnum)
  (->> id-ranges
       (mapcar (lambda (range) (range (car range) (cadr range))))
       (mapcar ($>> (remove-if-not #'invalid-id-2)))
       (mapcar ($>> (reduce #'+)))
       (reduce #'+)))

(let ((input (parse-input "2-input")))
  (->> input
       round-1
       (format t "Round 1: ~a~%"))
  (->> input
       round-2
       (format t "Round 2: ~a~%")))
