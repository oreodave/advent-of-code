(load "lib")

(defparameter input (uiop:read-file-string "11-input"))
(defparameter lines (get-lines input))

(defun lines->segments (lines)
  (--> (split-by-completely lines "" #'string=)
       (mapcar (lambda (x)
            (--> (loop for i in x
                       collect
                       (string-trim '(#\Space) i))))
          it)))

(defun segment->monkey (segment)
  (destructuring-bind (name starting op test clause-1 clause-2) segment
    (declare (ignore name))
    `(:items
      ,(--> (subseq starting 16)
            (string-to-clist it)
            (split-by-completely it #\,)
            (mapcar (lambda (lst)
                 (--> (clist-to-string lst)
                      (string-trim '(#\Space) it)
                      (parse-integer it)))
               it))
      :inspect
      ,(--> (subseq op 21)
            (format nil "(~a old)" it)
            (with-input-from-string (s it)
              (read s))
            (eval `(lambda (old) ,it)))
      :test
      ,(let ((test (parse-integer (subseq test 19)))
             (clause-1 (parse-integer (subseq clause-1 25)))
             (clause-2 (parse-integer (subseq clause-2 26))))
         (lambda (n)
           (if (= (mod n test) 0)
               clause-1
               clause-2)))
      :divisor ,(parse-integer (subseq test 19)))))

(defmacro get-monkey (monkeys ind)
  `(nth ,ind ,monkeys))

(defmacro get-monkey-items (monkeys ind)
  `(getf (get-monkey ,monkeys ,ind) :items))

(defun monkey-increment-alist (monkey-alist monkey-ind)
  (setf (cdr (assoc monkey-ind monkey-alist))
        (+ 1 (cdr (assoc monkey-ind monkey-alist)))))

(defun monkey-send-item (monkeys item recipient)
  (setf (get-monkey-items monkeys recipient)
        (append (get-monkey-items monkeys recipient)
                (list item))))

(defun monkey-process-item (inspect item)
  (floor (/ (funcall inspect item) 3)))

(defun monkey-do-turn (monkeys monkey-alist monkey-ind)
  (let* ((monkey  (nth monkey-ind monkeys))
         (items   (getf monkey :items))
         (inspect (getf monkey :inspect))
         (test    (getf monkey :test)))
    (loop for item in items
          for proper-item    = (monkey-process-item inspect item)
          for recipient      = (funcall test proper-item)
          do (monkey-send-item monkeys proper-item recipient)
          do (monkey-increment-alist monkey-alist monkey-ind))
    (setf (get-monkey-items monkeys monkey-ind) nil)
    monkeys))

(defun monkeys-do-round (monkeys monkey-alist)
  (loop for monkey in monkeys
        for i from 0
        do (monkey-do-turn monkeys monkey-alist i)))

(defun monkey-business (monkey-alist)
  (--> (lambda (x y) (> (cdr x) (cdr y)))
       (sort monkey-alist it)
       (subseq it 0 2)
       (mapcar #'cdr it)
       (reduce #'* it)))

(let* ((monkeys (--> (lines->segments lines)
                     (mapcar #'segment->monkey it)))
       (monkey-alist (loop for monkey in monkeys
                           for i from 0
                           collect `(,i . 0))))

  (loop for i from 1 to 20
        do (monkeys-do-round monkeys monkey-alist))

  (format t "Round 1: ~a~%" (monkey-business monkey-alist)))

(defvar *monkey-divisor* nil)

(defun monkey-process-item (inspect item)
  (mod (funcall inspect item) *monkey-divisor*))

(let* ((monkeys (--> (lines->segments lines)
                     (mapcar #'segment->monkey it)))
       (monkey-alist (loop for monkey in monkeys
                           for i from 0
                           collect `(,i . 0))))

  ;; To ensure we don't go too hard.
  (setq *monkey-divisor* (reduce #'*
                            (loop for monkey in monkeys
                                  for divisor = (getf monkey :divisor)
                                  collect divisor)))

  (loop for i from 1 to 10000
        do (monkeys-do-round monkeys monkey-alist))

  (format t "Round 2: ~a~%" (monkey-business monkey-alist)))
