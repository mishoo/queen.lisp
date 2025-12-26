(in-package #:queen)

(defgeneric parse-pgn (input &key ext-moves &allow-other-keys))

(defmethod parse-pgn ((in stream) &key ext-moves)
  (with-parse-stream in
    (labels
        ((read-sym ()
           (read-while #'alnum?))

         (read-header ()
           (let (name value)
             (skip #\[)
             (setf name (read-sym))
             (skip-whitespace)
             (setf value (read-string))
             (skip #\])
             (cons name value)))

         (read-moves (game &optional variant)
           (let ((data nil)
                 (prev-game nil))
             (flet ((done ()
                      (return-from read-moves (nreverse data)))
                    (move ()
                      (let* ((comp-moves (game-compute-moves game))
                             (valid (game-parse-san game in comp-moves)))
                        (skip-whitespace)
                        (cond
                          ((null valid)
                           (croak "Invalid move"))
                          ((< 1 (length valid))
                           (croak "Ambiguous move")))
                        (setf prev-game (copy-game game)
                              (game-board prev-game) (copy-seq (game-board game)))
                        (cond
                          (ext-moves
                           (push (list :move (car valid)
                                       :san (game-san game (car valid) comp-moves)
                                       :fen-before (game-fen game)
                                       :fen-after (progn
                                                    (game-move game (car valid))
                                                    (game-fen game)))
                                 data))
                          (t
                           (push (cons :move (car valid)) data)
                           (game-move game (car valid))))))
                    (comment1 ()
                      (read-while (lambda (ch)
                                    (not (eql #\Newline ch)))))
                    (comment2 ()
                      (prog1
                          (read-while (lambda (ch)
                                        (not (eql #\} ch))))
                        (skip #\}))))
               (loop for ch = (progn
                                (skip-whitespace)
                                (peek))
                     while ch do
                       (cond
                         ((eql ch #\;)
                          (next)
                          (push (cons :comment (comment1)) data))
                         ((eql ch #\{)
                          (next)
                          (push (cons :comment (comment2)) data))
                         ((eql ch #\()  ; variant
                          (next)
                          (let ((game (copy-game prev-game)))
                            (setf (game-board game)
                                  (copy-seq (game-board game)))
                            (push `(:variant . ,(read-moves game t)) data)))
                         ((eql ch #\))
                          (when variant
                            (next)
                            (done))
                          (croak "Unmatched close paren"))
                         ((eql ch #\$) ; $N annotations, not supported but skip silently
                          (next)
                          (read-integer))
                         ((eql ch #\*)  ; end - result unknown
                          (next)
                          (when variant
                            (skip-whitespace)
                            (skip #\)))
                          (done))
                         (t
                          (awhen (read-integer)
                            (cond
                              ((and (= it 0) (eql (peek) #\-)) ; 0-1
                               (skip "-1")
                               (done))
                              ((and (= it 1) (eql (peek) #\-)) ; 1-0
                               (skip "-0")
                               (done))
                              ((and (= it 1) (eql (peek) #\/)) ; 1-2/1-2
                               (skip "/2-1/2")
                               (done))
                              (t
                               (skip #\.)
                               (skip-whitespace)
                               (when (eql #\. (peek))
                                 (skip ".."))
                               (skip-whitespace))))
                          (move)))
                     finally (done))))))

      (let* ((headers (loop do (skip-whitespace)
                            while (eql #\[ (peek))
                            collect (read-header)))
             (game (make-game))
             (start-fen (assoc "fen" headers :test #'string-equal)))
        (reset-from-fen game (if start-fen
                                 (cdr start-fen)
                                 +FEN-START+))
        `(:headers ,headers
          :moves ,(read-moves game)
          :game ,game)))))

(defmethod parse-pgn ((pgn string) &rest args &key &allow-other-keys)
  (with-input-from-string (in pgn)
    (apply #'parse-pgn in args)))
