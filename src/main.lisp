(in-package :centi)

(let ((eof (gensym)))

  (defun load (path)
    (with-open-file (s path)
      (loop for form = (r:read :stream s :eof-marker eof)
            until (member form (list eof (s:intern "nil")))
            for value = (i:eval form b:*stdenv*))))

  (defun repl ()
    (loop
      for form = (r:read :stream nil
                         :eof-marker eof)
      until (member form (list eof (s:intern "nil")))
      do (i:eval (list (s:intern "print1") form) b:*stdenv*)
         (terpri))))

(defun load-stdlib ()
  (let ((paths '("/data/data/com.termux/files/home/centi-999/lisp"
                 "/home/geri/centi-999/lisp"))
        (files '("bootstrap" "stdlib")))
    (dolist (path paths)
      (when (uiop:directory-exists-p path)
        (dolist (file files)
          (load (format nil "~a/~a.centi" path file)))))))

(defun main (&rest args)
  (load-stdlib)
  (if (null args)
      (repl)
      (load (car args))))
