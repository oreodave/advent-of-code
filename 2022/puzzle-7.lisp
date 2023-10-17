(load "lib")
(defparameter input (uiop:read-file-string "7-input"))
(defparameter lines (get-lines input)) ;; first line is ALWAYS "$ cd /"

(defun tokenise-lines (lines)
  "Split each LINE in LINES by space"
  (mapcar (lambda (line)
       (mapcar #'clist-to-string
          (split-by-completely (string-to-clist line) #\Space)))
     lines))

(defun is-token-cmd? (token)
  "Checks if TOKEN is a command (by the first member)"
  (string= (car token) "$"))

(defun till-next-cmd (tokens)
  "Iterates over TOKENS till a command is found, returning the remaining
tokens (including command)."
  (loop
    for token-set on tokens
    if (is-token-cmd? (car token-set))
      return token-set))

;;; Functions to manage a directory structure
(progn
  #| What is a directory structure? You have some root directory, with
  some entries.  Your current working directory (CWD), at the start of
  the program, will be at the root but later on could be any descendant
  of the root.  How do we manage moving around the directory structure
  and ensure updates to the overall structure?

  We should maintain a HISTORY stack where
  CAR(HISTORY) = (NAME of CWD . State of CWD according to parent).

  So the top of the history stack represents the current directory
  structure in the form it'll inhabit in its parents.  We need a way to
  update the top of the history stack with the CWD.

  UPDATE-CURRENT-DIRECTORY(CWD, HISTORY) {
  CWD-ENTRY = PEEK(HISTORY);
  IF NULL(CWD-ENTRY)
  HISTORY = ((NIL . CWD))
  ELSE
  SETF((CDR CWD-ENTRY), CWD);
  }
  |#
  (defun update-current-directory (cwd history)
    "Updates the top of HISTORY (structure of CWD) with CWD i.e. set the
'state' of the top of the the HISTORY stack to CWD."
    (if (null (car history))
        (values
         cwd
         `((nil ,@cwd)))
        (progn
          (setf (cdr (car history)) cwd)
          (values
           cwd
           history))))

  #|
  Say CWD = {... a = directory, ...}.  How do I go down to `a`?

  DOWN-DIRECTORY(a, CWD, HISTORY) {

  UPDATE-CURRENT-DIRECTORY(CWD, HISTORY); <-- This is so we don't lose
  any current information
  HISTORY = ACONS(a, CWD@a, HISTORY);
  CWD     = CWD@a;
  }
  |#
  (defun down-directory (name cwd history)
    (multiple-value-bind (cwd history) (update-current-directory cwd history)
      (let ((cwd@a (cdr (assoc name cwd :test #'string=))))
        (values
         cwd@a
         (acons name cwd@a history)))))

  #|
  How do we ascend the directory structure?  We need to account for 3 cases:
  1) At the root
  2) Parent is the root
  3) Parent is any directory


  Then UP-DIRECTORY(CWD, HISTORY) {
  PAIR   = POP(HISTORY);               <-- (CWD-NAME . PREVIOUS-CWD-STATE)
  if NULL(CAR(PAIR))
  then error(AT ROOT)
  else
  PARENT = PEEK(HISTORY);              <-- Parent of CWD (PARENT-NAME . PARENT-STATE)
  ENTRY = ASSOC(CAR(PAIR) CDR(PARENT)) <-- Gives us the CWD entry in the parent
  SETF CDR(ENTRY) CWD                  <-- Update the parent
  CWD = CDR(PARENT)                    <-- Update CWD
  }
  |#
  (defun up-directory (cwd history)
    (when (null (caar history))
      ;; At root, so we can't go further up
      (error "Directory already at root"))
    ;; We can't use names as in the pseudocode because we need to mutate
    ;; this specific pointer: Lisp copies by value so we won't be
    ;; mutating it if we use names.
    (setf
     ;; CDR(ENTRY)
     (cdr
      ;; ENTRY
      (assoc
       ;; CAR(PAIR)
       (car (car history))
       ;; CDR(PARENT)
       (cdr (cadr history))
       :test #'string=))
     cwd)
    (values
     (cdr (cadr history))
     (cdr history)))

  #|
  Finally, how do we go from some child all the way back to the root?
  UP-TO-ROOT(CWD, HISTORY) {
  IF (NULL (CAAR HISTORY)) <-- Means we're at the root
  RETURN
  ELSE
  UP-DIRECTORY(CWD, HISTORY)
  UP-TO-ROOT(CWD, HISTORY)
  }
  |#
  (defun up-to-root (cwd history)
    (if (null (caar history))
        (values
         cwd
         history)
        (multiple-value-bind (cwd history) (up-directory cwd history)
          (up-to-root cwd history)))))

;;; Functions to parse the shell output
(progn
  (defun parse-ls (tokens)
    "Converts the following TOKENS till a command (via IS-TOKEN-CMD?) into a
directory structure i.e. an alist of string names by their content."
    (loop for token in tokens
          until (is-token-cmd? token)
          collect
          (let ((size (car token))
                (name (cadr token)))
            (if (string= size "dir")
                `(,name . nil)
                `(,name . ,(parse-integer size))))))

  (defun parse-cd (name cwd history)
    "Converts TOKEN into an action on CWD and HISTORY then returns them"
    (cond
      ((string= "/" name)
       (up-to-root cwd history))
      ((string= ".." name)
       (up-directory cwd history))
      (t
       (down-directory name cwd history))))

  (defun parse-next-command (tokens cwd history)
    "Parses the next command available in TOKENS, managing CWD and HISTORY.
Assumes CAR(TOKENS) is a command (see IS-TOKEN-CMD?)"
    (cond
      ((null tokens)
       (multiple-value-bind (cwd history) (up-to-root cwd history)
         (values
          tokens
          cwd
          history)))
      ((string= (cadar tokens) "ls")
       (values
        (till-next-cmd (cdr tokens))
        (parse-ls (cdr tokens))
        history))
      ((string= (cadar tokens) "cd")
       (multiple-value-bind (cwd history) (parse-cd (caddar tokens) cwd history)
         (values
          (till-next-cmd (cdr tokens))
          cwd
          history)))))

  (defun parse-all-tokens (tokens &optional cwd history)
    "Recursively parse all TOKENS, returning (CWD . HISTORY) from the
perspective of the root."
    (if (null tokens)
        (multiple-value-bind (cwd history) (up-to-root cwd history)
          (values
           cwd
           history))
        (multiple-value-bind (tokens cwd history)
            (parse-next-command tokens cwd history)
          (parse-all-tokens tokens cwd history)))))

(defun is-record-directory? (record)
  "Checks if the record (output from parse-tokens) is a directory by
checking if the CDR is a list."
  (listp (cdr record)))

(defun strip-file-names (record)
  "Deletes file names, leaving the corresponding file sizes and
directory names (and the wrapping list structure)."
  (if (is-record-directory? record)
      (cons (car record)
            (loop
              for entry in (cdr record)
              collect
              (strip-file-names entry)))
      (cdr record)))

(defun get-directory-sizes (dir &optional current)
  "Computes the size of the directory dir, as well as all subdirectories.
Returns both the overall size of dir as well as an alist of sizes for
subdirectories."
  (if (not (listp dir))
      (values
       dir
       nil)
      (let ((name (car dir))
            (size (loop
                    for record in (cdr dir)
                    sum
                    (multiple-value-bind (size dir-alist) (get-directory-sizes record nil)
                      ;; This is for the alist of subdirectory sizes
                      (setq current (concatenate 'list dir-alist current))
                      size))))
        (values
         size
         (acons name size current)))))

(defparameter TOTAL-SIZE 70000000)
(defparameter REQUESTED-SIZE 30000000)
(multiple-value-bind (_ history)
    (parse-all-tokens (tokenise-lines lines))
  ;; Since we're at the root, CAR(history) is the entire file structure
  (declare (ignore _))
  (multiple-value-bind (sum size-list)
      (get-directory-sizes
       (strip-file-names
        ;; normalize the name of root
        (cons "/" (cdr (car history)))))
    (format t "Round 1: ~s~%"
            (reduce #'+ (loop for rec in size-list
                              if (< (cdr rec) 100000) ; only records that are at most 100000
                                collect (cdr rec))))
    (let* ((root-size sum)
           (free-space (- TOTAL-SIZE root-size))
           (extra-space-req (- REQUESTED-SIZE free-space)))
      (format t "Round 2: ~s~%"
              (cdar
               (sort (loop
                       for rec in size-list
                       ;; only records whose size is at least the space required
                       if (>= (cdr rec) extra-space-req)
                         collect rec)
                     (lambda (rec1 rec2)
                       (< (cdr rec1) (cdr rec2)))))))))
