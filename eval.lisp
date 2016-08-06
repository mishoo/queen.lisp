(in-package #:cl-chess)

(in-readtable cl-chess::syntax)

(defconstant +MATK+ 10000)
(defconstant +MATQ+ 900)
(defconstant +MATR+ 500)
(defconstant +MATB+ 330)
(defconstant +MATN+ 320)
(defconstant +MATP+ 100)

(defparameter +MAX-DEPTH+ 5)

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
  (unless (is-white? piece)
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
     (+ +MATK+ (aref *k-scores-opening* row col)))))

(defun static-value (game)
  (let ((total 0)
        (our-side (game-side game)))
    (board-foreach
     (game-board game)
     (lambda (piece row col index)
       (declare (ignore index))
       (let ((score (get-score piece row col)))
         (if (same-side? piece our-side)
             (incf total score)
             (decf total score)))))
    total))

(defun move-value (move)
  (let ((score (piece-value (move-piece move))))
    (awhen (move-captured-piece move)
      (setf score (+ 10000 (piece-value it))))
    (awhen (move-promoted-piece move)
      (incf score (+ 15000 (piece-value it))))
    (when (move-check? move)
      (incf score 20000))
    score))

(defun sort-moves (moves)
  (stable-sort moves
               (lambda (m1 m2)
                 (> (move-value m1)
                    (move-value m2)))))

(defun quies-moves (moves)
  (sort-moves (remove-if-not (lambda (m)
                               (or (move-capture? m)
                                   (move-promote? m)
                                   ;; (move-check? m)
                                   ))
                             moves)))

(defun quies (game α β moves pline)
  (let ((score (static-value game)))
    (when (>= score β)
      (return-from quies β))
    (when (> score α)
      (setf α score))
    (if (null moves)
        (if (attacked? game)
            -15000
            (- α))
        (loop for move in (quies-moves moves)
              for line = (cons nil nil)
              do (with-move (game move t)
                   (setf score (- (quies game (- β) (- α)
                                         (game-compute-moves game)
                                         line))))
                 (when (>= score β)
                   (return β))
                 (when (> score α)
                   (setf α score)
                   (setf (car pline) (cons move (car line))))
              finally (return α)))))

(defun pvs (game depth α β pline)
  (declare (type game game)
           (type (unsigned-byte 8) depth)
           (type (integer -32000 32000) α β)
           (type cons pline))
  (let ((moves (sort-moves (game-compute-moves game))))
    (cond
      ((null moves)
       (if (attacked? game)
           -15000
           (- (static-value game))))
      ((zerop depth)
       (quies game α β moves pline))
      (t
       (loop with score = nil
             for line = (cons nil nil)
             for move in moves do
               (with-move (game move t)
                 (cond
                   (score
                    (setf score (- (pvs game (1- depth) (- 0 α 1) (- α) line)))
                    (when (< α score β)
                      (setf score (- (pvs game (1- depth) (- β) (- score) line)))))
                   (t
                    (setf score (- (pvs game (1- depth) (- β) (- α) line))))))
               (when (> score α)
                 (setf α score)
                 (setf (car pline) (cons move (car line))))
               (when (>= α β)
                 (return-from pvs α)))
       α))))

(defun find-best-move (game &optional (depth +MAX-DEPTH+))
  (let* ((line (cons nil nil))
         (score (pvs game depth -32000 +32000 line)))
    (values (car line) score)))

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
       (multiple-value-bind (line score) (find-best-move g)
         (let ((m (car line)))
           (cond
             (m
              (format t "*********** ~A ~A ~A~%" (if (is-white? (game-side g))
                                                     "W:" "B:")
                      (dump-line g line)
                      m)
              (format t "~A~%" (let ((*unicode* nil))
                                 (game-fen g)))
              (game-move g m)
              (print-board (game-board g))
              (format t "~A~%" (let ((*unicode* nil))
                                 (game-fen g))))
             (t
              (format t "No moves found~%")))))))))

(defun dump-line (game moves)
  (with-output-to-string (out)
    (labels ((rec (moves first)
               (when moves
                 (let ((move (car moves)))
                   (when (or first (move-white? move))
                     (unless first
                       (write-char #\SPACE out))
                     (format out "~D." (game-fullmove game)))
                   (when (and first (move-black? move))
                     (format out ".."))
                   (format out " ~A" (game-san game move))
                   (with-move (game move)
                     (rec (cdr moves) nil))))))
      (rec moves t))))

(defun dump-moves (game &optional (moves (game-compute-moves game)))
  (format nil "~{~A~^ ~}" (mapcar (lambda (m)
                                    (let* ((before (copy-array (game-board game)))
                                           (san (game-san game m))
                                           (after (copy-array (game-board game))))
                                      (cond
                                        ((not (equalp before after))
                                         (format nil "[~A]" san))
                                        (t
                                         san))))
                                  (remove-if #'null moves))))
