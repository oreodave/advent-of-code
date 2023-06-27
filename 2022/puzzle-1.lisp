(defvar input (uiop:read-file-string "1-input"))

(defvar *sep (format nil "~%~%"))

(defun parse-entity (inp)
  (with-input-from-string (s inp)
    (loop for line = (read-line s nil)
          while line
          collect (parse-integer line))))

(defun get-lists (input)
  (let* ((pos (search *sep input))
         (converted (parse-entity (subseq input 0 pos))))
    (if (null pos)
        (list converted)
        (cons converted
              (get-lists (subseq input (+ pos 2)))))))

(defvar sums (sort (mapcar (lambda (lst) (reduce #'+ lst)) (get-lists input))
                   #'>))

(format t "Round 1: ~a~%" (car sums))

(destructuring-bind (first second third &rest _) sums
  (format t "Round 2: ~a,~a,~a:>~a" first second third
          (+ first second third)))
