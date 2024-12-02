(load "util.lisp")

(--> (uiop:read-file-lines "1-input")
     (loop for line in _
           for x = (search "   " line)
           collect (parse-integer (subseq line 0 x)) into left
           collect (parse-integer (subseq line (+ x 3))) into right
           finally (return (list (sort left #'<) (sort right #'<))))
     (format t "Round 1: ~a~%Round 2: ~a~%"
             (loop for x in (car _)
                   for y in (cadr _)
                   sum (abs (- y x)))
             (loop for item in (car _)
                   for count = (count item (cadr _))
                   sum (* item count))))
