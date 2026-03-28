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

(defun load (path env k)
  (with-open-file (s (resolve-filepath path))
    (labels ((helper (value)
               (let ((form (read :stream s)))
                 (if (member form (list 'EOF (intern "nil")))
                     (funcall k value)
                     (evaluate form env #'helper)))))
      (helper (intern "nil")))))

(defun repl ()
  (loop
    with environment = (environment *stdenv*)
    for form = (read :stream nil)
    until (member form (list 'EOF (intern "nil")))
    do (evaluate (list (intern "print") form) environment #'identity)
       (terpri)))

(defun main ()
  (destructuring-bind (&optional file-path)
      uiop:*command-line-arguments*
    (init)
    (if file-path
        (if (uiop:file-exists-p file-path)
            (load file-path (environment *stdenv*) #'identity)
            (format uiop:*stderr*
                    "centi: file '~a' doesn't exist"
                    file-path))
        (repl))))
