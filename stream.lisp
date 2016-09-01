(in-package #:queen)

(defparameter *has-letter-prop* (cl-unicode:property-test "Letter"))

(defmacro with-parse-stream (input &body body)
  (let ((pos (gensym "pos"))
        (line (gensym "line"))
        (col (gensym "col"))
        (log (gensym "log"))
        (-next (gensym "-next")))
    `(let ((,pos 0) (,line 1) (,col 0) (,log nil))
       (labels
           ((,-next ()
              (if ,log
                  (pop ,log)
                  (read-char ,input nil 'EOF)))

            (unget (ch)
              (push ch ,log))

            (peek ()
              (if ,log
                  (car ,log)
                  (peek-char nil ,input nil nil)))

            (next ()
              (let ((ch (,-next)))
                (when (and (eql ch #\Return)
                           (eql (peek) #\Newline))
                  (incf ,pos)
                  (setf ch (,-next)))
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

            (look-ahead (len func)
              (let ((savepos ,pos)
                    (saveline ,line)
                    (savecol ,col))
                (let ((chars (loop repeat len collect (next))))
                  (or (funcall func chars)
                      (progn
                        (setf ,pos savepos
                              ,line saveline
                              ,col savecol
                              ,log (nconc chars ,log))
                        nil)))))

            (skip (ch &optional no-error)
              (cond
                ((characterp ch)
                 (let ((curr (next)))
                   (if (eql ch curr)
                       curr
                       (unless no-error
                         (croak "Expected ~A but found ~A" ch curr)))))
                ((stringp ch)
                 (let ((match
                           (look-ahead (length ch)
                                       (lambda (chars)
                                         (unless (member nil chars)
                                           (string= ch (coerce chars 'string)))))))
                   (if match match
                       (unless no-error
                         (croak "Expected ~A ch")))))
                (t
                 (error "Unknown token in `skip'"))))

            (read-number ()
              (let (n d ch)
                (tagbody
                 next
                   (setq ch (next))
                   (unless ch (go finish))
                   (setq d (digit? ch))
                   (unless d (go finish))
                   (setf n (+ d (* (or n 0) 10)))
                   (go next)
                 finish
                   (when ch (unget ch)))
                n))

            (read-string (&optional (quote #\") (esc #\\))
              (skip quote)
              (let* ((escaped nil))
                (prog1 (read-while (lambda (ch)
                                     (cond
                                       (escaped
                                        (setf escaped nil)
                                        t)
                                       ((eql ch quote)
                                        nil)
                                       ((eql ch esc)
                                        (next)
                                        (setf escaped t)
                                        t)
                                       (t t))))
                  (skip quote))))

            (skip-whitespace ()
              (read-while #'whitespace?)))

         (declare (ignorable #'peek
                             #'next
                             #'unget
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
                             #'look-ahead
                             #'read-number))

         (unwind-protect
              (progn ,@body)
           (when ,log
             (unread-char (car ,log) ,input)))))))
