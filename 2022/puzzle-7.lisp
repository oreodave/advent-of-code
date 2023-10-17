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

#| What is a directory structure? You have some root directory, with
some entries.  Your current working directory (CWD), at the start of
the program, will be at the root but later on could be any descendant
of the root.  How do we manage moving around the directory structure
and ensure updates to the root?

We should maintain a HISTORY stack where
CAR(HISTORY) = (NAME of CWD . State of CWD when entering).

Firstly a function which updates the HISTORY on our current state.
UPDATE-CURRENT-DIRECTORY(CWD, HISTORY) {
CWD-ENTRY = PEEK(HISTORY);
IF NULL(CWD-ENTRY)
HISTORY = ((NIL . CWD))
ELSE
SETF((CDR CWD-ENTRY), CWD);
}

Say CWD = {... a = directory, b = file, ...}
DOWN-DIRECTORY(a, CWD, HISTORY) {
UPDATE-CURRENT-DIRECTORY(CWD, HISTORY); <-- This is so we don't lose information
HISTORY = ACONS(a, CWD@a, HISTORY);
CWD     = CWD@a;
}


Then UP-DIRECTORY(CWD, HISTORY) {
PAIR   = POP(HISTORY);               <-- (CWD-NAME . PREVIOUS-CWD-STATE)
PARENT = PEEK(HISTORY);              <-- Parent of CWD (PARENT-NAME . PARENT-STATE)
ENTRY = ASSOC(CAR(PAIR) CDR(PARENT)) <-- Gives us the CWD entry in the parent
SETF CDR(ENTRY) CWD                  <-- Update the parent
CWD = CDR(PARENT)                    <-- Update CWD
}
|#

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
