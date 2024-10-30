(load "lib")

(defparameter input (uiop:read-file-string "10-input"))
(defparameter lines (get-lines input))

(defun parse-line (line)
  (destructuring-bind (op . operand)
      (--> (string-to-clist line)
           (split-by-completely it #\Space)
           (mapcar #'clist-to-string it))
    (if (string= op "noop")
        '(noop . nil)
        `(add . ,(parse-integer (car operand))))))

(defparameter important-cycles (list 20 60 100 140 180 220))

(defun check-sum (sum cycles x)
  (if (member cycles important-cycles)
      (+ sum (* cycles x))
      sum))

(let ((sum 0))
  (loop with x = 1
        for cycles from 1
        for line in lines
        for (op . operand) = (parse-line line)
        if (eq op 'noop)
          do (setq sum (check-sum sum cycles x))
        else
          do (setq sum (check-sum sum cycles x)
                   cycles (+ cycles 1)
                   sum (check-sum sum cycles x)
                   x (+ x operand)))
  (format t "Round 1: ~a~%" sum))

(let ((lit-pixels nil))
  (defun update-pixels (x cycles)
    (let ((cycles (mod (- cycles 1) 40)))
      (push
       (if (or (= cycles (+ x 1))
               (= cycles x)
               (= cycles (- x 1)))
           "#"
           ".")
       lit-pixels)))

  (loop with x = 1
        for cycles from 1
        for line in lines
        for (op . operand) = (parse-line line)
        if (eq op 'noop)
          do (update-pixels x cycles)
        else
          do (update-pixels x cycles)
          and do (setq cycles (+ cycles 1))
          and do (update-pixels x cycles)
          and do (setq x (+ x operand)))

  (setq lit-pixels (reverse lit-pixels))

  (format t "Round 2: ")
  (loop for pixel in lit-pixels
        for cycle from 0
        if (= (mod cycle 40) 0)
          do (format t "~%")
        do
        (format t "~a" pixel)))
