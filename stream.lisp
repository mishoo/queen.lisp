(in-package #:queen)

(defparameter *has-letter-prop* (cl-unicode:property-test "Letter"))

(defmacro with-parse-stream (input &body body)
  (let ((pos (gensym "pos"))
        (line (gensym "line"))
        (col (gensym "col")))
    `(let ((,pos 0) (,line 1) (,col 0))
       (labels
           ((peek ()
              (peek-char nil ,input nil nil))

            (next ()
              (let ((ch (read-char ,input nil 'EOF)))
                (when (and (eql ch #\Return)
                           (eql (peek) #\Newline))
                  (incf ,pos)
                  (setf ch (read-char ,input nil 'EOF)))
                (case ch
                  (EOF
                   nil)
                  (#\Newline
                   (setf ,col 0)
                   (incf ,line)
                   (incf ,pos)
                   ch)
                  (otherwise
                   (incf ,col)
                   (incf ,pos)
                   ch))))

            (eof? ()
              (not (peek)))

            (croak (msg &rest args)
              (when args
                (setf msg (apply #'format nil msg args)))
              (error "ERROR: ~A (~A:~A)" msg ,line ,col))

            (read-while (pred)
              (with-output-to-string (out)
                (loop for ch = (peek)
                      while (and ch (funcall pred ch))
                      do (write-char (next) out))))

            (whitespace? (ch)
              (case (char-code ch)
                ((32 10 13 9 #xA0 #xFEFF) t)))

            (non-whitespace? (ch)
              (not (whitespace? ch)))

            (digit? (ch)
              (digit-char-p ch))

            (letter? (ch)
              (funcall *has-letter-prop* ch))

            (alnum? (ch)
              (or (digit? ch)
                  (letter? ch)))

            (skip (ch &optional no-error)
              (cond
                ((characterp ch)
                 (let ((curr (next)))
                   (if (eql ch curr)
                       curr
                       (unless no-error
                         (croak "Expected ~A but found ~A" ch curr)))))
                ((stringp ch)
                 (let* ((i -1)
                        (n (length ch))
                        (val (read-while (lambda (curr)
                                           (and (< (incf i) n)
                                                (eql curr (char ch i)))))))
                   (if (= i n) val
                       (unless no-error
                         (croak "Expected ~A but found ~A" ch val)))))
                (t
                 (error "Unknown token in `skip'"))))

            (read-integer ()
              (let ((str (read-while #'digit?)))
                (unless (zerop (length str))
                  (parse-integer str))))

            (read-string (&optional (quote #\") (esc #\\))
              (skip quote)
              (read-while (lambda (ch)
                            (cond
                              ((eql ch esc)
                               (next)
                               (or (peek)
                                   (error "Unexpected EOF reading string")))
                              ((eql ch quote)
                               (next)
                               nil)
                              (t t)))))

            (skip-whitespace ()
              (read-while #'whitespace?)))

         (declare (ignorable #'peek
                             #'next
                             #'eof?
                             #'croak
                             #'read-while
                             #'whitespace?
                             #'non-whitespace?
                             #'digit?
                             #'letter?
                             #'alnum?
                             #'skip
                             #'read-string
                             #'skip-whitespace
                             #'read-integer))

         ,@body))))
