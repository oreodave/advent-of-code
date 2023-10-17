(load "lib")
(defparameter input (uiop:read-file-string "7-input"))
(defparameter lines (get-lines input)) ;; first line is ALWAYS "$ cd /"

(defun tokenise-lines (lines)
  "Split each LINE in LINES by space"
  (mapcar (lambda (line)
       (mapcar #'clist-to-string
          (split-completely (string-to-clist line) #\Space)))
     lines))

(defun token-cmdp (token)
  "Checks if TOKEN is a command (by the first member)"
  (string= (car token) "$"))

(defun till-next-cmd (tokens)
  "Iterates over TOKENS till a command is found, returning the remaining
tokens (including command)."
  (loop
    for token-set on tokens
    if (token-cmdp (car token-set))
      return token-set))

(defun parse-ls (tokens)
  "Converts the following TOKENS till a command (via TOKEN-CMDP) into a
directory structure i.e. an alist of string names by their content."
  (loop for token in tokens
        until (token-cmdp token)
        collect
        (let ((size (car token))
              (name (cadr token)))
          (if (string= size "dir")
              `(,name . nil)
              `(,name . ,(parse-integer size))))))

(defun parse-tokens (tokens)
  (if (token-cmdp (car tokens))
      (cond
        ((string= (cadar tokens) "ls")
         (values
          (till-next-cmd (cdr tokens))
          (parse-ls (cdr tokens))))
        ((string= (cadar tokens) "cd")
         ;; TODO: Actually figure out what I should do here
         (values
          (cdr tokens)
          nil)))))
