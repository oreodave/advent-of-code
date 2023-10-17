(load "lib")
(defparameter input (uiop:read-file-string "8-input"))
(defparameter lines (get-lines input))
;; Just converts every line into a sequence of integral digits
(defparameter trees
  (mapcar (lambda (line) (mapcar (lambda (char) (parse-integer (string char))) (string-to-clist line)))
     lines))
