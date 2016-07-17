(in-package #:cl-chess)

(in-readtable cl-chess::syntax)

(defconstant +MATK+ 20000)
(defconstant +MATQ+ 900)
(defconstant +MATR+ 500)
(defconstant +MATB+ 330)
(defconstant +MATN+ 320)
(defconstant +MATP+ 100)

(defmacro defscore (name &body value)
  `(defparameter ,name
     (make-array '(8 8) :element-type '(byte 8)
                        :initial-contents
                        (reverse ',value))))

(defscore *p-scores*
  ( 0   0   0   0   0   0   0   0)
  (50  50  50  50  50  50  50  50)
  (10  10  20  30  30  20  10  10)
  ( 5   5  10  25  25  10   5   5)
  ( 0   0   0  20  20   0   0   0)
  ( 5  -5 -10   0   0 -10  -5   5)
  ( 5  10  10 -20 -20  10  10   5)
  ( 0   0   0   0   0   0   0   0))

(defscore *n-scores*
  (-50 -40 -30 -30 -30 -30 -40 -50)
  (-40 -20   0   0   0   0 -20 -40)
  (-30   0  10  15  15  10   0 -30)
  (-30   5  15  20  20  15   5 -30)
  (-30   0  15  20  20  15   0 -30)
  (-30   5  10  15  15  10   5 -30)
  (-40 -20   0   5   5   0 -20 -40)
  (-50 -40 -30 -30 -30 -30 -40 -50))

(defscore *b-scores*
  (-20 -10 -10 -10 -10 -10 -10 -20)
  (-10   0   0   0   0   0   0 -10)
  (-10   0   5  10  10   5   0 -10)
  (-10   5   5  10  10   5   5 -10)
  (-10   0  10  10  10  10   0 -10)
  (-10  10  10  10  10  10  10 -10)
  (-10   5   0   0   0   0   5 -10)
  (-20 -10 -10 -10 -10 -10 -10 -20))

(defscore *r-scores*
  ( 0   0   0   0   0   0   0   0)
  ( 5  10  10  10  10  10  10   5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  ( 0   0   0   5   5   0   0   0))

(defscore *q-scores*
  (-20 -10 -10  -5  -5 -10 -10 -20)
  (-10   0   0   0   0   0   0 -10)
  (-10   0   5   5   5   5   0 -10)
  ( -5   0   5   5   5   5   0  -5)
  (  0   0   5   5   5   5   0  -5)
  (-10   5   5   5   5   5   0 -10)
  (-10   0   5   0   0   0   0 -10)
  (-20 -10 -10  -5  -5 -10 -10 -20))

(defscore *k-scores-opening*
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-20 -30 -30 -40 -40 -30 -30 -20)
  (-10 -20 -20 -20 -20 -20 -20 -10)
  ( 20  20   0   0   0   0  20  20)
  ( 20  30  10   0   0  10  30  20))

(defscore *k-scores-ending*
  (-50 -40 -30 -20 -20 -30 -40 -50)
  (-30 -20 -10   0   0 -10 -20 -30)
  (-30 -10  20  30  30  20 -10 -30)
  (-30 -10  30  40  40  30 -10 -30)
  (-30 -10  30  40  40  30 -10 -30)
  (-30 -10  20  30  30  20 -10 -30)
  (-30 -30   0   0   0   0 -30 -30)
  (-50 -30 -30 -30 -30 -30 -30 -50))

(defun count-pieces (board color)
  (declare (type board board)
           (type piece color))
  (let ((q1 0) (r1 0) (b1 0) (n1 0) (p1 0)
        (q2 0) (r2 0) (b2 0) (n2 0) (p2 0)
        (dbl1 0) (dbl2 0)
        (colpawns1 (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
        (colpawns2 (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    (board-foreach board
                   (lambda (piece row col)
                     (declare (type piece piece)
                              (type (unsigned-byte 3) row col)
                              (ignore row))
                     (macrolet ((same ()
                                  `(same-side? piece color)))
                       (cond
                         ((is-queen? piece) (if (same) (incf q1) (incf q2)))
                         ((is-rook? piece) (if (same) (incf r1) (incf r2)))
                         ((is-bishop? piece) (if (same) (incf b1) (incf b2)))
                         ((is-knight? piece) (if (same) (incf n1) (incf n2)))
                         ((is-pawn? piece)
                          (cond
                            ((same)
                             (incf p1)
                             (when (> (incf (aref colpawns1 col)) 1)
                               (incf dbl1)))
                            (t
                             (incf p2)
                             (when (> (incf (aref colpawns2 col)) 1)
                               (incf dbl2)))))))))
    (values q1 r1 b1 n1 p1
            q2 r2 b2 n2 p2
            dbl1 dbl2)))

(defun static-value (game &optional
                            (our-side (game-side game))
                            (moves (game-compute-moves game)))
  (when (null moves)
    ;; we've been checkmated
    (return-from static-value -10000))
  (let* ((their-side (logxor our-side +WHITE+))
         (material (multiple-value-bind (q1 r1 b1 n1 p1
                                         q2 r2 b2 n2 p2)
                       (count-pieces game our-side)
                     (+ (* 9 (- q1 q2))
                        (* 5 (- r1 r2))
                        (* 3 (+ (- b1 b2)
                                (- n1 n2)))
                        (- p1 p2)))))

    ))
