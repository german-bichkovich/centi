(in-package :centi.reader)

(defun delimiter? (c)
  (member c '(#\space #\tab #\newline #\) #\] #\} nil)))

(defun whitespace? (c)
  (member c '(#\newline #\space #\tab)))

;; TODO read macros with #func ...
;; #read-hex a8b9
;; #read-octal 361100
;; #read-binary 1010101010
(defun read (&key (stream *standard-input*) eof-marker)
  (labels ((make-buffer ()
             (make-array 0 :fill-pointer 0
                           :adjustable t
                           :element-type 'character))
           (next () (read-char stream nil))
           (peek () (peek-char nil stream nil))
           (read-escape ()
             (next)  ; #\\
             (let ((c (next)))
               (case c
                 (#\r #\return)
                 (#\e #\escape)
                 (#\n #\newline)
                 (#\t #\tab)
                 (t c))))
           (skip-whitespace (depth)
             (loop for c = (peek) do
               (cond ((whitespace? c)
                      (next))
                     ((eq c #\#)
                      (if (eq (next) #\;)
                          (read1 depth)
                          (loop until (member (next)
                                              '(nil #\newline)))))
                     (t
                      (return)))))
           (read-string ()
             (next)  ; #\"
             (loop with string = (make-buffer)
                   for c = (peek)
                   do (cond ((eq c #\")
                             (next)
                             (return string))
                            ((null c)
                             (error "read: unterminated string"))
                            (t
                             (vector-push-extend (if (eq c #\\)
                                                     (read-escape)
                                                     (next))
                                                 string)))))
           (read-quote (depth)
             (next)  ; #\'
             (list (s:intern "quote") (read1 depth)))
           (read-atom ()
             (loop with name = (make-buffer)
                   for c = (peek)
                   do (if (delimiter? c)
                          (return
                            (handler-case (parse-integer name)
                              (error () (s:intern name))))
                          (vector-push-extend (if (eq c #\\)
                                                  (read-escape)
                                                  (next))
                                              name))))
           (read-seq (delimiter depth)
             (next)
             (loop with result = (cons nil nil)
                   with current = result
                   do (skip-whitespace depth)
                      (if (eq (peek) delimiter)
                          (progn (next)
                                 (return (cdr result)))
                          (let ((thing (read1 depth)))
                            (cond ((eq thing (s:intern "."))
                                   (rplacd current (read1 depth))
                                   (skip-whitespace depth)
                                   (if (eq (next) delimiter)
                                       (return (cdr result))
                                       (error
                                        "read-list: malformed pair")))
                                  (t
                                   (setf (cdr current) (cons thing nil)
                                         current (cdr current))))))))
           (read-list (depth)
             (read-seq #\) depth))
           (read-vector-form (depth)
             (cons (s:intern "vector")
                   (read-seq #\] depth)))
           (read-hashmap-form (depth)
             (cons (s:intern "hashmap")
                   (read-seq #\} depth)))
           (read1 (depth)
             (incf depth)
             (skip-whitespace depth)
             (case (peek)
               ((#\) #\} #\])
                (error "read: unexpected '~a'" (next)))
               ((nil) (if (= depth 0)
                          eof-marker
                          (error "read: unexpected EOF")))
               (#\(   (read-list depth))
               (#\[   (read-vector-form depth))
               (#\{   (read-hashmap-form depth))
               (#\'   (read-quote depth))
               (#\"   (read-string))
               (t     (read-atom)))))
    (read1 -1)))

(defun read-string (string)
  (with-input-from-string (s string)
    (read :stream s :eof-marker 'eof)))
