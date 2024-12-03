(defmacro --> (first &rest functions)
  (if (null functions)
      first
      `(let* ,(loop :for f :in (cons first functions)
                    appending `((_ ,f)))
         _)))

(defun search-all (substr str &optional acc len)
  (let ((x (search substr str))
        (len (or len 0)))
    (if (null x)
        (reverse acc)
        (search-all substr (subseq str (1+ x))
                    (cons (+ x len) acc)
                    (+ len x 1)))))

  (defun zip (a b)
    (loop for i in a
          for j in b
          collect (cons i j)))
