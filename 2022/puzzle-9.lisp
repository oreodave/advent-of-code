(load "lib")

(defparameter input (uiop:read-file-string "9-input"))
(defparameter lines (get-lines input))

(defun coords-eq? (x y)
  (and (= (car x) (car y)) (= (cdr x) (cdr y))))

(defun move (P X Y)
  (cons (+ (car P) X) (+ (cdr P) Y)))

(defun -1unit (x)
  (if (< x 0)
      (+ x 1)
      (- x 1)))

(defun update-tail (TAIL HEAD)
  (let ((xd (- (car HEAD) (car TAIL)))
        (yd (- (cdr HEAD) (cdr TAIL))))
    (cond
      ;; Still touching
      ((and (<= (abs xd) 1) (<= (abs yd) 1)) TAIL)
      ;; These do the diagonal one steps
      ((and (= 1 (abs xd)) (= 2 (abs yd)))
       (move TAIL xd (-1unit yd)))
      ((and (= 1 (abs yd)) (= 2 (abs xd)))
       (move TAIL (-1unit xd) yd))
      ;; Do the diagonal 2 steps (may happen on round 2)
      ((and (= 2 (abs xd)) (= 2 (abs yd)))
       (move TAIL (-1unit xd) (-1unit yd)))
      ;; Linear movements on an axis
      ((= (abs xd) 2)
       (move TAIL (-1unit xd) 0))
      ((= (abs yd) 2)
       (move TAIL 0 (-1unit yd))))))

(defun parse-line (line)
  (cons (intern (subseq line 0 1))
        (parse-integer (subseq line 2))))

(defun move-direction (direction point)
  (case direction
    (R (move point 1 0))
    (U (move point 0 1))
    (L (move point -1 0))
    (D (move point 0 -1))))

(let nil
  (defun execute-line (line head tail points)
    (destructuring-bind (direction . magnitude) (parse-line line)
      (dotimes (x magnitude)
        (setq head (move-direction direction head))
        (setq tail (update-tail tail head))
        (setq points (adjoin tail points :test #'coords-eq?))))
    (values head tail points))

  (defun execute-lines (lines &optional head tail points)
    (if (null lines)
        (values
         head tail points)
        (let ((line (car lines))
              (head (or head `(0 . 0)))
              (tail (or tail `(0 . 0)))
              (points (or points nil)))
          (multiple-value-bind (head tail points) (execute-line line head tail points)
            (execute-lines (cdr lines) head tail points)))))

  (format t "round 1: ~s~%"
          (length (car (last (multiple-value-list (execute-lines lines)))))))

(let nil
  (defun update-knots (head knots)
    (let ((new-knots (list (update-tail (car knots) head))))
      (loop for knot in (cdr knots)
            do
               (setq new-knots (cons (update-tail knot (car new-knots)) new-knots)))
      new-knots))

  (defun execute-line (line head knots points)
    (destructuring-bind (direction . magnitude) (parse-line line)
      (dotimes (x magnitude)
        (setq head (move-direction direction head))
        (setq knots (update-knots head knots))
        (setq points (adjoin (car knots) points :test #'coords-eq?))
        (setq knots (reverse knots))))
    (values head knots points))

  (defun execute-lines (lines)
    (let ((head `(0 . 0))
          (knots (loop for x from 1 to 9 collect `(0 . 0)))
          (points nil))
      (loop for line in lines
            do
               (multiple-value-bind (nhead nknots npoints)
                   (execute-line line head knots points)
                 (setq head nhead
                       knots nknots
                       points npoints)))
      (values
       head knots points)))
  (format t "Round 2: ~a~%" (length (car (last (multiple-value-list (execute-lines lines)))))))
