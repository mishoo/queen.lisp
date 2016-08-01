(in-package #:cl-chess)

(in-readtable cl-chess::syntax)

(defconstant +MATK+ 10000)
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

(defun piece-value (piece)
  (case (piece piece)
    (#.+PAWN+   +MATP+)
    (#.+KNIGHT+ +MATN+)
    (#.+BISHOP+ +MATB+)
    (#.+ROOK+   +MATR+)
    (#.+QUEEN+  +MATQ+)
    (#.+KING+   +MATK+)))

(defun get-score (piece row col)
  (let ((white (is-white? piece)))
    (unless white
      (setf row (- 7 row)
            col (- 7 col)))
    (case (piece piece)
      (#.+PAWN+   (+ +MATP+ (aref *p-scores* row col)))
      (#.+KNIGHT+ (+ +MATN+ (aref *n-scores* row col)))
      (#.+BISHOP+ (+ +MATB+ (aref *b-scores* row col)))
      (#.+ROOK+   (+ +MATR+ (aref *r-scores* row col)))
      (#.+QUEEN+  (+ +MATQ+ (aref *q-scores* row col)))
      (#.+KING+
       ;; XXX: handle endgame
       (+ +MATK+ (aref *k-scores-opening* row col))))))

(defun static-value (game &optional
                            (our-side (game-side game))
                            ;; (moves (game-compute-moves game our-side))
                            )
  (let ((total 0))
    (board-foreach
     (game-board game)
     (lambda (piece row col)
       (let ((score (get-score piece row col)))
         (if (same-side? piece our-side)
             (incf total score)
             (decf total score)))))
    total))

(defparameter *best-line* nil)

(defparameter +MAX-DEPTH+ 4)

(defun quies-moves (game moves)
  (sort (delete-if-not (lambda (m)
                         (or (move-capture? m)
                             ;; (with-move (game m)
                             ;;  (attacked? game))
                             ))
                       moves)
        (lambda (m1 m2)
          (cond
            ((and (move-capture? m1)
                  (move-capture? m2))
             (let ((p1 (move-piece m1))
                   (p2 (move-piece m2))
                   (c1 (move-captured-piece m1))
                   (c2 (move-captured-piece m2)))
               (let ((v1 (- (piece-value c1)
                            (piece-value p1)))
                     (v2 (- (piece-value c2)
                            (piece-value p2))))
                 (> v1 v2))))))))

(defun quies (game α β &optional moves)
  (unless (king-index game)
    (return-from quies -15000))
  (unless moves
    (setf moves (game-compute-moves game (game-side game) t)))
  (setf moves (quies-moves game moves))
  ;; (format t "~A:~A ~A~%" α β (dump-moves game moves))
  ;; (print-board (game-board game))
  (let ((score (static-value game)))
    (when (>= score β)
      (return-from quies β))
    (when (> score α)
      (setf α score))
    (loop for move in moves
          do (with-move (game move)
               (setf score (- (quies game (- β) (- α)))))
             (when (>= score β)
               (return β))
             (when (> score α)
               (setf α score))
          finally (return α))))

(defun pvs (game depth α β)
  (declare (type game game)
           (type (unsigned-byte 8) depth)
           (type (integer -32000 32000) α β))
  (let ((moves (game-compute-moves game (game-side game)
                                   (/= depth +MAX-DEPTH+))))
    (cond
      ((or (zerop depth)
           (null moves))
       (quies game α β moves))
      (t
       (loop with score = nil for move in moves do
         (with-move (game move)
           (cond
             (score
              (setf score (- (pvs game (1- depth) (- 0 α 1) (- α))))
              (when (< α score β)
                (setf score (- (pvs game (1- depth) (- β) (- score))))))
             (t
              (setf score (- (pvs game (1- depth) (- β) (- α)))))))
         (when (> score α)
           (setf α score)
           (when (= depth +MAX-DEPTH+)
             (setf *best-line* move)))
         (when (>= α β)
           (return-from pvs α)))
       α))))

(defun find-best-move (game)
  (let ((*best-line* nil))
    (let ((score (pvs game +MAX-DEPTH+ -32000 +32000)))
      (values *best-line* score))))

;;; XXX: rest is debug code

(defvar g)
(setf g (make-instance 'game))
(reset-game g)
(defun go! (&optional m)
  (let ((*unicode* t))
    (cond
      (m
       (let ((moves (game-parse-san g m)))
         (cond
           ((= 1 (length moves))
            (game-move g (car moves)))
           (t
            (format t "Ambiguous: ~{~A ~}~%"
                    (mapcar (lambda (m)
                              (game-san g m))
                            moves))))
         (print-board (game-board g))))
      (t
       (let ((m (find-best-move g)))
         (cond
           (m
            (format t "*********** ~A ~A~%" (if (is-white? (game-side g))
                                                "W:" "B:")
                    (game-san g m))
            (game-move g m)
            (print-board (game-board g)))
           (t
            (format t "No moves found~%"))))))))

(defun dump-moves (game &optional (moves (game-compute-moves game)))
  (format nil "~{~A~^ ~}" (mapcar (lambda (m)
                                    (game-san game m))
                                  moves)))
