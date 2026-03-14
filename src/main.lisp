(in-package :centi)

(defun resolve-filepath (p)
  (cond ((uiop:string-prefix-p "./" p)
         (format nil "~a~a.centi" (uiop:getcwd) (subseq p 2)))
        ((eq (aref p 0) #\@)
         (format nil
                 "~a~a.centi"
                 (uiop:getenv "CENTI_HOME")
                 (subseq p 1)))
        ((eq (aref p 0) #\/)
         (format nil "~a.centi" p))
        (t
         (error "resolve-filepath: bad string ~a" p))))

(defun load (path &optional in-stdenv?)
  (with-open-file (s (resolve-filepath path))
    (loop for form = (read :stream s)
          until (member form (list 'EOF (intern "nil")))
          for value = (eval form (if in-stdenv?
                                     *stdenv*
                                     (environment *stdenv*)))
          finally (return value))))

(defun repl ()
  (loop
    for form = (read :stream nil)
    until (member form (list 'EOF (intern "nil")))
    do (eval (list (intern "print") form) *stdenv*)
       (terpri)))

(defun load-stdlib ()
  (load "@/lisp/bootstrap" t))

(defun main ()
  (destructuring-bind (&optional file-path)
      uiop:*command-line-arguments*
    (load-stdlib)
    (if file-path
        (if (uiop:file-exists-p file-path)
            (load file-path)
            (error "centi: file doesn't exist ~a" file-path))
        (repl))))
