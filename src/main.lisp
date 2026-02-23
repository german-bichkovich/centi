(in-package :centi)

(let ((eof (gensym)))

  (defun load (path)
    (with-open-file (s path)
      (loop for form = (read :stream s :eof-marker eof)
            until (member form (list eof (intern "nil")))
            for value = (eval form *stdenv*))))

  (defun repl ()
    (loop
      for form = (read :stream nil
                       :eof-marker eof)
      until (member form (list eof (intern "nil")))
      do (eval (list (intern "print") form) *stdenv*)
         (terpri))))

(defun load-stdlib ()
  (let ((home (uiop:getenv "CENTI_HOME"))
        (files '("bootstrap" "stdlib")))
    (dolist (file files)
      (load (format nil "~a/lisp/~a.centi" home file)))))

(defun main ()
  (destructuring-bind (&optional file-path)
      uiop:*command-line-arguments*
    (load-stdlib)
    (if file-path
        (if (uiop:file-exists-p file-path)
            (load file-path)
            (error "centi: file doesn't exist ~a" file-path))
        (repl))))
