(defpackage "aoc-util"
  (:use :cl)
  (:export
   :=>
   :--> :->> :-<>
   :$-> :$<> :$>>
   :alist-val :call-rev :fn
   :split :rev-map :parse-integer* :remove-at-indices :range))

(in-package "aoc-util")

(deftype => (args result)
  "Type level DSL for function types"
  `(function ,(typecase args
                (null args)
                (symbol (list args))
                (otherwise args))
             ,result))

(defmacro --> (placeholder &body forms)
  "Lexically bind current form as `placeholder' for use in the next form, returning
the result of the last form.

i.e.

(--> (a1 a2...) (b1 b2...) (c1 c2...)) =
(let* ((placeholder (a1 a2 ...))
       (placeholder (b1 b2 ...))
       (placeholder (c1 c2 ...)))
    _ )

Also includes transformer where symbols are considered unary functions i.e.
(--> x y) <-> (--> x (y placeholder)).
"
  (if (null forms)
      nil
      (let ((assignment-forms
              (loop :for i :from 0
                    :for f :in forms
                    :for canon-f := (if (and (> i 0) (symbolp f))
                                        (list f placeholder)
                                        f)
                    :collect `(,placeholder ,canon-f))))
        `(let* ,assignment-forms
           ,placeholder))))

(defmacro ->> (&rest forms)
  "Make current form the last argument of the next form, returning the last
  form.

i.e.
(->> (a1 a2...) (b1 b2...) (c1 c2...)) == (c1 c2 ... (b1 b2 ... (a1 a2 ...)))

Also includes transformer where symbols are considered unary functions.

Like the `|>' operator in Ocaml."
  (if (null forms)
      nil
    (loop :with acc = (car forms)
          :for func :in (cdr forms)
          :for canon-func = (if (symbolp func) (list func) func)
          :do (setq acc (append canon-func (list acc)))
          :finally (return acc))))

(defmacro -<> (&rest forms)
  "Make current form the first argument of the next form, returning the last
  form.

i.e.
(-<> (a1 a2...) (b1 b2...) (c1 c2...)) == (c1 (b1 (a1 a2 ...) b2 ...) c2 ...)

Also includes transformer where symbols are considered unary functions.

Like the `|>' operator in Ocaml."
  (if (null forms)
      nil
      (loop :with acc = (car forms)
            :for func :in (cdr forms)
            :for canon-func = (if (symbolp func) (list func) func)
            :do (push acc (cdr canon-func))
            :do (setq acc canon-func)
            :finally (return acc))))

(defmacro $-> (capture &rest forms)
  "Given a sequence of FORMS, return a unary function which applies each form
sequentially via -->"
  `(lambda (,capture)
     (--> ,capture ,capture ,@forms)))

(defmacro $<> (&rest forms)
  "Given a sequence of FORMS, return a unary function which applies each form
sequentially via -<>"
  (let ((capture (gensym)))
    `(lambda (,capture)
       (-<> ,capture ,@forms))))

(defmacro $>> (&rest forms)
  "Given a sequence of FORMS, return a unary function which applies each form
sequentially via ->>"
  (let ((capture (gensym)))
    `(lambda (,capture)
       (->> ,capture ,@forms))))

(defmacro alist-val (key alist &key (test #'eq))
  "Helper macro for getting the value of KEY in ALIST."
  `(cdr (assoc ,key ,alist :test ,test)))

(defmacro call-rev (func-name &rest arguments)
  "Call a function with arguments but in reverse
i.e. (call-rev f x1 x2 ... xn) => (f xn ... x2 x1)."
  `(,func-name ,@(reverse arguments)))

(defmacro fn (name lambda-list type &body body)
  "Construct a function `NAME' with a declared function type `TYPE' that takes
arguments `LAMBDA-LIST' with body `BODY'."
  `(progn
     (declaim (ftype ,type ,name))
     (defun ,name ,lambda-list
       ,@body)))

(fn split (n lst) (=> (fixnum sequence) (values sequence sequence))
    "Return CONS where CAR is the first N elements of LST and CDR is the rest."
    (if (< (length lst) n)
        (values nil nil)
        (values (subseq lst 0 n)
                (subseq lst n))))

(fn rev-map (indicator lst &key (test #'eq))
    (=> (function sequence &key (:test function)) list)
    "Given LST and INDICATOR: LST -> A, return an association list A -> 2^LST
where key x in A has associations {y in LST : INDICATOR(y) = x}."
    (loop :with assoc-list := nil
          :for element :in (coerce lst 'list)
          :for key := (funcall indicator element)
          :for value := (cdr (assoc key assoc-list :test test))
          :if value
            :do (setf (alist-val key assoc-list :test test)
                      (cons element value))
          :else
            :do (setq assoc-list (-<> (list key element) (cons assoc-list)))
          :finally (return assoc-list)))

(fn parse-integer* (inp) (=> string (or integer list))
    "Given string INP, attempt to parse an integer.  Return NIL otherwise."
    (parse-integer inp :junk-allowed t))

(fn remove-at-indices (indices lst) (=> (list sequence) list)
  "Given a set of INDICES and a list LST, return a copy of LST without items at any
index in INDICES."
  (loop :for i :from 0 :to (1- (length lst))
        :for item :in (coerce lst 'list)
        :if (not (member i indices))
          :collect item))

(fn range (lower upper) (=> (fixnum fixnum) list)
  (loop for i from lower to upper
        collect i))
