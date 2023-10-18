(load "lib")
(defparameter input (uiop:read-file-string "9-input"))
(defparameter lines (get-lines input))

(defun move (P X Y)
  (cons (+ (car P) X) (+ (cdr P) Y)))

(defun -1unit (x)
  (if (< x 0)
      (+ x 1)
      (- x 1)))

(defun update-tail (TAIL HEAD)
  (let ((xd (- (car HEAD) (car TAIL)))
        (yd (- (cdr HEAD) (cdr TAIL))))
    (if (and (<= (abs xd) 1) (<= (abs yd) 1))
        ;; Leave as is
        TAIL
        (cond
          ;; Diagonal movements
          ((and (= 1 (abs xd)) (= 2 (abs yd)))
           (move TAIL xd (-1unit yd)))
          ((and (= 1 (abs yd)) (= 2 (abs xd)))
           (move TAIL (-1unit xd) yd))
          ;; Linear movements on an axis
          ((= (abs xd) 2)
           (move TAIL (-1unit xd) 0))
          ((= (abs yd) 2)
           (move TAIL 0 (-1unit yd)))))))

(defun move-direction (direction P)
  (case direction
    (R (move P 1 0))
    (L (move P -1 0))
    (U (move P 0 1))
    (D (move P 0 -1))))

(defun print-grid (head tail)
  (destructuring-bind (hx . hy) head
    (destructuring-bind (tx . ty) tail
      (loop for x from 0 to 6
            do
               (format t "-"))
      (format t "~%")
      (loop for y from 6 downto 0
            do
               (progn
                 (format t "~d: " y)
                 (loop for x from 0 to 6
                       do
                          (format t "~s"
                                  (cond
                                    ((and (= hx x) (= hy y)) 'H)
                                    ((and (= tx x) (= ty y)) 'T)
                                    (t '-))))
                 (format t "~%"))))))

(defun parse-line (line)
  (cons (intern (subseq line 0 1))
        (parse-integer (subseq line 2))))

(defun coords-eq? (x y)
  (and (= (car x) (car y)) (= (cdr x) (cdr y))))

(defun single-knot-execute-line (line head tail points)
  (destructuring-bind (direction . magnitude) (parse-line line)
    (dotimes (x magnitude)
      (setq head (move-direction direction head))
      (setq tail (update-tail tail head))
      (setq points (adjoin tail points :test #'coords-eq?))))
  (values head tail points))

(defun single-knot-execute-lines (lines &optional head tail points)
  (if (null lines)
      (values
       head tail points)
      (let ((line (car lines))
            (head (or head `(0 . 0)))
            (tail (or tail `(0 . 0)))
            (points (or points nil)))
        (multiple-value-bind (head tail points) (single-knot-execute-line line head tail points)
          (single-knot-execute-lines (cdr lines) head tail points)))))

(format t "round 1: ~s~%"
        (length (car (last (multiple-value-list (single-knot-execute-lines lines))))))
