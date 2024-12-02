(defmacro --> (first &rest functions)
  (if (null functions)
      first
      `(let* ,(loop :for f :in (cons first functions)
                    appending `((_ ,f)))
         _)))

(defun zip (a b)
  (loop for i in a
        for j in b
        collect (cons i j)))
