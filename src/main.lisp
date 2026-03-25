(in-package :centi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((default-home-directory (uiop:getcwd)))
    (defun resolve-filepath (p)
      (cond ((uiop:string-prefix-p "./" p)
             (format nil
                     "~a~a.centi"
                     (uiop:getcwd)
                     (subseq p 2)))
            ((eq (aref p 0) #\@)
             (format nil
                     "~a~a.centi"
                     (or (uiop:getenv "CENTI_HOME")
                         default-home-directory)
                     (subseq p 1)))
            ((eq (aref p 0) #\/)
             (format nil "~a.centi" p))
            (t
             (error "resolve-filepath: bad string ~a" p))))))

(defun load (path &optional in-stdenv?)
  (with-open-file (s (resolve-filepath path))
    (loop with env = (if in-stdenv?
                         *stdenv*
                         (environment *stdenv*))
          for form = (read :stream s)
          until (member form (list 'EOF (intern "nil")))
          for value = (evaluate form env)
          finally (return value))))

(defun repl ()
  (loop
    with environment = (environment *stdenv*)
    for form = (read :stream nil)
    until (member form (list 'EOF (intern "nil")))
    do (evaluate (list (intern "print") form) environment)
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
