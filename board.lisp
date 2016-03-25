(in-package #:cl-chess)

(in-readtable cl-chess::syntax)

(defconstant +QUEEN+      #x01)
(defconstant +ROOK+       #x02)
(defconstant +KNIGHT+     #x04)
(defconstant +BISHOP+     #x08)
(defconstant +PAWN+       #x10)
(defconstant +KING+       #x20)

(defconstant +PROMOTABLE+ #x0f)
(defconstant +CAPTURABLE+ #x1f)
(defconstant +PIECE+      #x3f)

(defconstant +WHITE+      #x40)
(defconstant +BLACK+      #x00)

(defparameter +FEN-START+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defparameter *unicode* nil)

(deftype piece ()
  '(unsigned-byte 7))

(deftype board-index ()
  '(and unsigned-byte (satisfies index-valid?)))

(deftype board ()
  '(array piece (8 8)))

(defun piece-side (p)
  (declare (type piece p))
  (logand p +WHITE+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-dammit (&rest chars)
    (coerce chars 'string))

  (defun index-valid? (index)
    (and (typep index '(unsigned-byte 8))
         (zerop (logand index #x88))))

  (defun white (p)
    (declare (type piece p))
    (logior p +WHITE+))

  (defun board-index (row col)
    (declare (type (integer 0 7) row col))
    (the board-index (logior col (ash row 4))))

  (defun field-index (field)
    (declare (type (simple-string 2) field))
    (let ((col (aref field 0))
          (row (aref field 1)))
      (board-index (- (char-code row) 49)
                   (- (logior 32 (char-code col)) 97))))

  (set-macro-character
   #\$ (lambda (stream ch)
         (declare (ignore ch))
         (field-index (string-dammit (read-char stream)
                                     (read-char stream))))))

(defun index-row (index)
  (declare (type board-index index))
  (ash (logand #x70 index) -4))

(defun index-col (index)
  (declare (type board-index index))
  (logand #x07 index))

(defmacro with-row-col ((index row col) &body body)
  (once-only (index)
    `(let ((,row (index-row ,index))
           (,col (index-col ,index)))
       (declare (ignorable ,row ,col))
       ,@body)))

(defun index-field (index)
  (declare (type board-index index))
  (with-row-col (index row col)
    (format nil "~C~C" (code-char (+ col 97)) (code-char (+ row 49)))))

(defun piece-char (p)
  (declare (type piece p))
  (if *unicode*
      (piece-unicode p)
      (ecase p
        ;; empty
        (0 #\-)
        ;; black
        (#.+PAWN+ #\p)
        (#.+KNIGHT+ #\n)
        (#.+KING+ #\k)
        (#.+BISHOP+ #\b)
        (#.+ROOK+ #\r)
        (#.+QUEEN+ #\q)
        ;; white
        (#.(white +PAWN+) #\P)
        (#.(white +KNIGHT+) #\N)
        (#.(white +KING+) #\K)
        (#.(white +BISHOP+) #\B)
        (#.(white +ROOK+) #\R)
        (#.(white +QUEEN+) #\Q))))

(defun piece-unicode (p)
  (declare (type piece p))
  (ecase p
    ;; empty
    (0 #\WHITE_SQUARE)
    ;; black
    (#.+PAWN+ #\BLACK_CHESS_PAWN)
    (#.+KNIGHT+ #\BLACK_CHESS_KNIGHT)
    (#.+KING+ #\BLACK_CHESS_KING)
    (#.+BISHOP+ #\BLACK_CHESS_BISHOP)
    (#.+ROOK+ #\BLACK_CHESS_ROOK)
    (#.+QUEEN+ #\BLACK_CHESS_QUEEN)
    ;; white
    (#.(white +PAWN+) #\WHITE_CHESS_PAWN)
    (#.(white +KNIGHT+) #\WHITE_CHESS_KNIGHT)
    (#.(white +KING+) #\WHITE_CHESS_KING)
    (#.(white +BISHOP+) #\WHITE_CHESS_BISHOP)
    (#.(white +ROOK+) #\WHITE_CHESS_ROOK)
    (#.(white +QUEEN+) #\WHITE_CHESS_QUEEN)))

(defun char-piece (p)
  (declare (type character p))
  (case p
    (#\- 0)
    ;; black
    ((#\p #\BLACK_CHESS_PAWN) +PAWN+)
    ((#\n #\BLACK_CHESS_KNIGHT) +KNIGHT+)
    ((#\k #\BLACK_CHESS_KING) +KING+)
    ((#\b #\BLACK_CHESS_BISHOP) +BISHOP+)
    ((#\r #\BLACK_CHESS_ROOK) +ROOK+)
    ((#\q #\BLACK_CHESS_QUEEN) +QUEEN+)
    ;; white
    ((#\P #\WHITE_CHESS_PAWN) #.(white +PAWN+))
    ((#\N #\WHITE_CHESS_KNIGHT) #.(white +KNIGHT+))
    ((#\K #\WHITE_CHESS_KING) #.(white +KING+))
    ((#\B #\WHITE_CHESS_BISHOP) #.(white +BISHOP+))
    ((#\R #\WHITE_CHESS_ROOK) #.(white +ROOK+))
    ((#\Q #\WHITE_CHESS_QUEEN) #.(white +QUEEN+))))

(defun is-pawn? (p)
  (declare (type piece p))
  (not (zerop (logand p +PAWN+))))

(defun is-knight? (p)
  (declare (type piece p))
  (not (zerop (logand p +KNIGHT+))))

(defun is-bishop? (p)
  (declare (type piece p))
  (not (zerop (logand p +BISHOP+))))

(defun is-rook? (p)
  (declare (type piece p))
  (not (zerop (logand p +ROOK+))))

(defun is-queen? (p)
  (declare (type piece p))
  (not (zerop (logand p +QUEEN+))))

(defun is-king? (p)
  (declare (type piece p))
  (not (zerop (logand p +KING+))))

(defun is-white? (p)
  (declare (type piece p))
  (not (zerop (logand p +WHITE+))))

(defun is-black? (p)
  (declare (type piece p))
  (not (is-white? p)))

(defun same-side? (p1 p2)
  (declare (type piece p1 p2))
  (= (logand p1 +WHITE+) (logand p2 +WHITE+)))

(defun opp-side? (p1 p2)
  (declare (type piece p1 p2))
  (not (same-side? p1 p2)))

(defun board-get (board index)
  (declare (type board board)
           (type board-index index))
  (with-row-col (index row col)
    (aref board row col)))

(defun board-set (board index val)
  (declare (type board board)
           (type board-index index)
           (type unsigned-byte val))
  (with-row-col (index row col)
    (setf (aref board row col) val)))

(defsetf board-get board-set)

(defmacro with-piece ((board pos p &optional allow-empty) &body body)
  (once-only (board pos)
    `(let ((,p (board-get ,board ,pos)))
       ,(if allow-empty
            `(progn ,@body)
            `(unless (zerop ,p)
               ,@body)))))

(defun make-board ()
  (make-array '(8 8) :element-type 'piece :initial-element 0))

(defun board-foreach (board fn)
  (declare (type board board)
           (type (function (piece (unsigned-byte 3) (unsigned-byte 3)) t) fn))
  (loop for row from 0 to 7 do
    (loop for col from 0 to 7
          for piece = (aref board row col)
          when (not (zerop piece))
            do (funcall fn piece row col))))

(defun print-board (board &key (output t))
  (loop for row from 7 downto 0
        for line = (loop for col from 0 to 7
                         collect (piece-char (aref board row col)))
        do (format output "~D │ ~{~A ~}~%" (+ row 1) line))
  (format output "  └─────────────────~%")
  (format output "    a b c d e f g h~%"))

(defconstant +WHITE-OO+     1)
(defconstant +WHITE-OOO+    2)
(defconstant +WHITE-CASTLE+ 3)
(defconstant +BLACK-OO+     4)
(defconstant +BLACK-OOO+    8)
(defconstant +BLACK-CASTLE+ 12)
(defconstant +ALL-CASTLE+   15)

(defclass game ()
  ((board :type board :initarg :board :accessor game-board)
   (state :type (unsigned-byte 32) :initarg :state :accessor game-state)
   (side :type (unsigned-byte 8) :initarg :side :accessor game-side)
   (enpa :type (or board-index null) :initarg :enpa :accessor game-enpa)
   (fullmove :type unsigned-byte :initarg :fullmove :accessor game-fullmove)
   (halfmove :type unsigned-byte :initarg :halfmove :accessor game-halfmove))
  (:default-initargs :board (make-board)
                     :state (logior +WHITE-OO+ +WHITE-OOO+ +BLACK-OO+ +BLACK-OOO+)
                     :side +WHITE+
                     :enpa nil
                     :fullmove 0))

(defmethod reset-from-fen ((game game) (in stream))
  (let ((board (game-board game))
        (state 0))
    (labels ((peek ()
               (peek-char nil in))

             (read-row (row)
               (loop for ch = (read-char in)
                     for col upfrom 0
                     do (cond
                          ((find ch "pnkbrqPNKBRQ")
                           (setf (aref board row col)
                                 (char-piece ch)))
                          ((find ch "12345678")
                           (dotimes (i (- (char-code ch) 48))
                             (setf (aref board row col) 0)
                             (incf col))
                           (decf col))
                          (t (unread-char ch in)
                             (return)))))

             (read-position ()
               (loop for row from 7 downto 0
                     do (read-row row)
                        (unless (zerop row)
                          (skip #\/))))

             (read-number ()
               (loop with n = 0
                     for ch = (read-char in nil nil)
                     while ch
                     for d = (digit-char-p ch)
                     while d do (setf n (+ d (* n 10)))
                     finally (unread-char ch in) (return n)))

             (read-side ()
               (case (read-char in)
                 (#\w (setf (game-side game) +WHITE+))
                 (#\b (setf (game-side game) +BLACK+))
                 (otherwise (error "Cannot read playing side"))))

             (read-castling ()
               (if (char= (peek) #\-)
                   (read-char in)
                   (loop while (find (peek) "kqKQ")
                         do (case (read-char in)
                              (#\k (setf state (logior state +BLACK-OO+)))
                              (#\q (setf state (logior state +BLACK-OOO+)))
                              (#\K (setf state (logior state +WHITE-OO+)))
                              (#\Q (setf state (logior state +WHITE-OOO+))))))
               (setf (game-state game) state))

             (read-en-passant ()
               (if (char= (peek) #\-)
                   (progn
                     (read-char in)
                     (setf (game-enpa game) nil))
                   (let ((col (read-char in))
                         (row (read-char in)))
                     (unless (and (find col "abcdefghABCDEFGH")
                                  (find row "12345678"))
                       (error "Invalid en-passant field"))
                     (setf (game-enpa game)
                           (board-index (- (char-code row) 49)
                                        (- (char-code (char-downcase col)) 97))))))

             (read-halfmove ()
               (setf (game-halfmove game) (read-number)))

             (read-fullmove ()
               (setf (game-fullmove game) (read-number)))

             (skip (ch)
               (if (char= (peek) ch)
                   (read-char in)
                   (error "Expected ~S" ch))))

      ;; now do it
      (read-position)
      (skip #\SPACE)
      (read-side)
      (skip #\SPACE)
      (read-castling)
      (skip #\SPACE)
      (read-en-passant)
      (skip #\SPACE)
      (read-halfmove)
      (skip #\SPACE)
      (read-fullmove)
      game)))

(defmethod reset-from-fen ((game game) (fen string))
  (with-input-from-string (in fen)
    (reset-from-fen game in)))

(defmethod reset-game ((game game))
  (reset-from-fen game +FEN-START+))

(defmethod game-fen ((game game))
  (let ((board (game-board game))
        (state (game-state game))
        (enpa (game-enpa game)))
    (with-output-to-string (*standard-output*)
      (loop for row from 7 downto 0
            do (loop with empty = 0
                     for col from 0 to 7
                     for p = (aref board row col)
                     do (cond
                          ((zerop p) (incf empty))
                          (t
                           (unless (zerop empty)
                             (format t "~D" empty))
                           (setf empty 0)
                           (write-char (piece-char p))))
                     finally (unless (zerop empty)
                               (format t "~D" empty)))
               (unless (zerop row)
                 (write-char #\/)))
      (write-char #\SPACE)
      (write-char (if (= 0 (game-side game)) #\b #\w))
      (write-char #\SPACE)
      (when (logtest state +WHITE-OO+) (write-char #\K))
      (when (logtest state +WHITE-OOO+) (write-char #\Q))
      (when (logtest state +BLACK-OO+) (write-char #\k))
      (when (logtest state +BLACK-OOO+) (write-char #\q))
      (unless (logtest state +ALL-CASTLE+) (write-char #\-))
      (write-char #\SPACE)
      (if enpa
          (write-string (index-field enpa))
          (write-char #\-))
      (write-char #\SPACE)
      (format t "~D" (game-halfmove game))
      (write-char #\SPACE)
      (format t "~D" (game-fullmove game)))))

;;; moves

(deftype move ()
  '(unsigned-byte 32))

(defun make-move (from to piece &key (enpa 0) (capture 0) (promo 0))
  (declare (type board-index from to)
           (type piece piece promo capture)
           (type (unsigned-byte 1) enpa))
  (let ((promo (logand promo +PROMOTABLE+))
        (capture (logand capture +CAPTURABLE+))
        (move 0))
    (with-row-col (from row col)
      (setf move (dpb col (byte 3 0) move))
      (setf move (dpb row (byte 3 3) move)))
    (with-row-col (to row col)
      (setf move (dpb col (byte 3 6) move))
      (setf move (dpb row (byte 3 9) move)))
    (setf move (dpb piece (byte 7 12) move))
    (setf move (dpb promo (byte 4 19) move))
    (setf move (dpb capture (byte 5 23) move))
    (setf move (dpb enpa (byte 1 28) move))
    (the move move)))

(defun move-from (move)
  (declare (type move move))
  (board-index (ldb (byte 3 3) move)
               (ldb (byte 3 0) move)))

(defun move-to (move)
  (declare (type move move))
  (board-index (ldb (byte 3 9) move)
               (ldb (byte 3 6) move)))

(defun move-piece (move)
  (declare (type move move))
  (ldb (byte 7 12) move))

(defun move-black? (move)
  (declare (type move move))
  (zerop (ldb (byte 1 18) move)))

(defun move-white? (move)
  (declare (type move move))
  (not (move-black? move)))

(defun move-side (move)
  (declare (type move move))
  (if (move-black? move) +BLACK+ +WHITE+))

(defun move-capture? (move)
  (not (zerop (ldb (byte 5 23) move))))

(defun move-captured-piece (move)
  (declare (type move move))
  (let ((p (ldb (byte 5 23) move)))
    (cond
      ((zerop p) nil)
      ((move-black? move) (logior p +WHITE+))
      (t p))))

(defun move-promote? (move)
  (not (zerop (ldb (byte 4 19) move))))

(defun move-promoted-piece (move)
  (declare (type move move))
  (let ((p (ldb (byte 4 19) move)))
    (cond
      ((zerop p) nil)
      ((move-black? move) p)
      (t (logior p +WHITE+)))))

(defun move-set-promoted-piece (move promo)
  (declare (type move move)
           (type piece promo))
  (dpb (logand promo +PROMOTABLE+) (byte 4 19) move))

(defun move-enpa? (move)
  (declare (type move move))
  (not (zerop (ldb (byte 1 28) move))))

(defun move-captured-index (move)
  (declare (type move move))
  (cond
    ((move-enpa? move)
     (board-index (ldb (byte 3 3) move)
                  (ldb (byte 3 6) move)))
    ((move-capture? move)
     (move-to move))))

(defun move-oo? (move)
  (declare (type move move))
  (and (is-king? (move-piece move))
       (= 2 (- (move-to move)
               (move-from move)))))

(defun move-ooo? (move)
  (declare (type move move))
  (and (is-king? (move-piece move))
       (= 2 (- (move-from move)
               (move-to move)))))

(defun move-castle? (move)
  (declare (type move move))
  (and (is-king? (move-piece move))
       (= 2 (abs (- (move-from move)
                    (move-to move))))))


;;; move execution

(defmethod game-move ((game game) move)
  (declare (type move move))
  (let ((board (game-board game))
        (from (move-from move))
        (to (move-to move))
        (piece (move-piece move))
        (white (move-white? move))
        (black (move-black? move))
        (promo (move-promoted-piece move)))
    ;; update board
    (board-set board to (or promo piece))
    (board-set board from 0)
    ;; handle special moves (castle and en-passant)
    (cond
      ((move-oo? move)
       (cond
         (white
          (board-set board $H1 0)
          (board-set board $F1 #.(white +ROOK+)))
         (t
          (board-set board $H8 0)
          (board-set board $F8 +ROOK+))))
      ((move-ooo? move)
       (cond
         (white
          (board-set board $A1 0)
          (board-set board $D1 #.(white +ROOK+)))
         (t
          (board-set board $A8 0)
          (board-set board $D8 +ROOK+))))
      ((move-enpa? move)
       (board-set board (move-captured-index move) 0)))
    ;; update game state
    (with-slots (state enpa side halfmove fullmove) game
      (setf side (if white +BLACK+ +WHITE+)
            enpa (when (and (is-pawn? piece)
                            (= (abs (- from to)) 32))
                   (ash (+ from to) -1)))
      (unless white
        (incf fullmove))
      (if (or (is-pawn? piece) (move-capture? move))
          (setf halfmove 0)
          (incf halfmove))
      (cond
        ((is-king? piece)
         (let ((castle (if white +WHITE-CASTLE+ +BLACK-CASTLE+)))
           (setf state (logxor (logior state castle) castle))))
        ((or (and white (= from $A1)) (and black (= to $A1)))
         (setf state (logxor (logior state +WHITE-OOO+) +WHITE-OOO+)))
        ((or (and white (= from $H1)) (and black (= to $H1)))
         (setf state (logxor (logior state +WHITE-OO+) +WHITE-OO+)))
        ((or (and black (= from $A8)) (and white (= to $A8)))
         (setf state (logxor (logior state +BLACK-OOO+) +BLACK-OOO+)))
        ((or (and black (= from $H8)) (and white (= to $H8)))
         (setf state (logxor (logior state +BLACK-OO+) +BLACK-OO+)))))))

(defmethod game-undo-move ((game game) move)
  (declare (type move move))
  (setf (game-side game) (move-side move))
  (let ((board (game-board game))
        (from (move-from move))
        (to (move-to move))
        (captured (move-captured-piece move))
        (piece (move-piece move))
        (white (move-white? move)))
    ;; restore board
    (board-set board from piece)
    (cond
      (captured
       (board-set board (move-captured-index move) captured)
       (when (move-enpa? move)
         (board-set board to 0)))
      (t
       (board-set board to 0)))
    ;; special moves
    (cond
      ((move-oo? move)
       (cond
         (white
          (board-set board $H1 #.(white +ROOK+))
          (board-set board $F1 0))
         (t
          (board-set board $H8 +ROOK+)
          (board-set board $F8 0))))
      ((move-ooo? move)
       (cond
         (white
          (board-set board $A1 #.(white +ROOK+))
          (board-set board $D1 0))
         (t
          (board-set board $A8 +ROOK+)
          (board-set board $D8 0)))))))

(defmacro with-move ((game move) &body body)
  (once-only (game move)
    (with-gensyms (state enpa halfmove fullmove)
      `(let ((,state (game-state ,game))
             (,enpa (game-enpa ,game))
             (,halfmove (game-halfmove ,game))
             (,fullmove (game-fullmove ,game)))
         (game-move ,game ,move)
         (unwind-protect
              (progn ,@body)
           (game-undo-move ,game ,move)
           (setf (game-state ,game) ,state
                 (game-enpa ,game) ,enpa
                 (game-halfmove ,game) ,halfmove
                 (game-fullmove ,game) ,fullmove))))))

;;; move generation

(defparameter +MOVES-KNIGHT+ '(31 33 14 18 -18 -14 -33 -31))
(defparameter +MOVES-BISHOP+ '(15 17 -15 -17))
(defparameter +MOVES-ROOK+   '(1 16 -16 -1))
(defparameter +MOVES-QING+   `(,@+MOVES-BISHOP+ ,@+MOVES-ROOK+))

(defmethod king-index ((game game) &optional (side (game-side game)))
  (let ((king (logior +KING+ side)))
    (board-foreach
     (game-board game)
     (lambda (p row col)
       (when (= p king)
         (return-from king-index (board-index row col)))))))

(defmethod attacked? ((game game) &optional
                                    (side (game-side game))
                                    (index (king-index game side)))
  (declare (type board-index index))
  (let* ((board (game-board game))
         (opp (logxor side +WHITE+)))
    (labels ((test (p piece)
               (when (and (same-side? p opp)
                          (= p (logand p piece)))
                 (return-from attacked? t)))
             (check (piece delta)
               (let ((pos (+ index delta)))
                 (when (index-valid? pos)
                   (with-piece (board pos p)
                     (test p piece)))))
             (repeat (piece delta)
               (loop for pos = (+ index delta) then (+ pos delta)
                     while (index-valid? pos)
                     do (with-piece (board pos p)
                          (test p piece)
                          (return)))))
      (cond ((is-white? opp)
             (check (white +PAWN+) -15)
             (check (white +PAWN+) -17))
            (t
             (check +PAWN+ +15)
             (check +PAWN+ +17)))
      (loop for delta in +MOVES-KNIGHT+ do
        (check (logior opp +KNIGHT+) delta))
      (loop for delta in +MOVES-BISHOP+ do
        (repeat (logior opp +BISHOP+ +QUEEN+) delta))
      (loop for delta in +MOVES-ROOK+ do
        (repeat (logior opp +ROOK+ +QUEEN+) delta))
      (loop for delta in +MOVES-QING+ do
        (check (logior opp +KING+) delta))
      nil)))

(defmethod game-compute-moves ((game game))
  (let* ((side (game-side game))
         (opp (logxor side +WHITE+))
         (board (game-board game))
         (white (is-white? side))
         (moves '())
         (enpa (game-enpa game))
         (my-king (king-index game side)))
    (board-foreach
     (game-board game)
     (lambda (piece row col)
       (when (same-side? piece side)
         (let ((from (board-index row col)))
           (labels ((move-pawn ()
                      (let ((on-end (= row (if white 6 1))))
                        (flet ((try-promo (move)
                                 (if (and on-end move)
                                     (add-promotions (pop moves))
                                     move)))
                          (cond (white
                                 (unless (try-promo (move +15 t))
                                   (try-enpa +15))
                                 (unless (try-promo (move +17 t))
                                   (try-enpa +17))
                                 (when (try-promo (move +16 nil t))
                                   (when (= row 1)
                                     (move +32 nil t))))
                                (t
                                 (unless (try-promo (move -15 t))
                                   (try-enpa -15))
                                 (unless (try-promo (move -17 t))
                                   (try-enpa -17))
                                 (when (try-promo (move -16 nil t))
                                   (when (= row 6)
                                     (move -32 nil t))))))))

                    (try-enpa (delta)
                      (when enpa
                        (let ((to (+ from delta)))
                          (when (= to enpa)
                            (add (make-move from to piece
                                            :capture (logior +PAWN+ opp)
                                            :enpa 1))))))

                    (add-promotions (move)
                      (push (move-set-promoted-piece move +KNIGHT+) moves)
                      (push (move-set-promoted-piece move +BISHOP+) moves)
                      (push (move-set-promoted-piece move +ROOK+) moves)
                      (push (move-set-promoted-piece move +QUEEN+) moves)
                      nil)

                    (move-knight ()
                      (mapc #'move +MOVES-KNIGHT+))

                    (move-bishop ()
                      (mapc #'repeat +MOVES-BISHOP+))

                    (move-rook ()
                      (mapc #'repeat +MOVES-ROOK+))

                    (move-queen ()
                      (mapc #'repeat +MOVES-QING+))

                    (move-king ()
                      (mapc #'move +MOVES-QING+)
                      (unless (attacked? game side my-king)
                        (cond
                          (white
                           (when (logtest (game-state game) +WHITE-OO+)
                             (try-castle $G1 $F1))
                           (when (logtest (game-state game) +WHITE-OOO+)
                             (try-castle $C1 $D1 $B1)))
                          (t
                           (when (logtest (game-state game) +BLACK-OO+)
                             (try-castle $G8 $F8))
                           (when (logtest (game-state game) +BLACK-OOO+)
                             (try-castle $C8 $D8 $B8))))))

                    (try-castle (&rest targets)
                      (loop for index in targets
                            unless (zerop (board-get board index))
                              do (return-from try-castle nil))
                      (unless (or (attacked? game side (first targets))
                                  (attacked? game side (second targets)))
                        (add (make-move from (car targets) piece))))

                    (%move (to &optional capture no-capture)
                      (when (index-valid? to)
                        (with-piece (board to p t)
                          (unless (and capture (zerop p))
                            (unless (and no-capture (not (zerop p)))
                              (when (or (zerop p) (opp-side? p side))
                                (when (logtest p +KING+)
                                  (error "Illegal position: opponent in check"))
                                (add (make-move from to piece :capture p))))))))

                    (move (delta &optional capture no-capture)
                      (%move (+ from delta) capture no-capture))

                    (repeat (delta)
                      (loop for to = (+ from delta) then (+ to delta)
                            do (%move to)
                            while (and (index-valid? to)
                                       (zerop (board-get board to)))))

                    (add (move)
                      (car (push move moves))))

             (case (logand piece +PIECE+)
               (#.+PAWN+   (move-pawn))
               (#.+KNIGHT+ (move-knight))
               (#.+BISHOP+ (move-bishop))
               (#.+ROOK+   (move-rook))
               (#.+QUEEN+  (move-queen))
               (#.+KING+   (move-king))))))))

    (delete-if (lambda (m)
                 (with-move (game m)
                   (let ((index (if (is-king? (move-piece m))
                                    (move-to m)
                                    my-king)))
                     (attacked? game side index))))
               moves)))

(defmethod game-parse-san ((game game) (in stream)
                           &optional (moves (game-compute-moves game)))
  (let* ((side (game-side game))
         (white (is-white? side))
         (promo nil)
         (capture nil)
         piece from to from-file from-rank to-file to-rank)
    (flet ((matches (m)
             (let ((mfrom (move-from m))
                   (mto (move-to m)))
               (when (and from to)
                 (return-from matches
                   (when (and (= from mfrom) (= to mto)
                              (eq promo (move-promoted-piece m)))
                     m)))
               (when (and (eq piece (move-piece m))
                          (eq promo (move-promoted-piece m)))
                 (with-row-col (mfrom mfromrow mfromcol)
                   (with-row-col (mto mtorow mtocol)
                     (when (and from-file (/= from-file mfromcol))
                       (return-from matches nil))
                     (when (and from-rank (/= from-rank mfromrow))
                       (return-from matches nil))
                     (when (and to-file (/= to-file mtocol))
                       (return-from matches nil))
                     (when (and to-rank (/= to-rank mtorow))
                       (return-from matches nil))
                     (when (eq capture t)
                       (return-from matches (and (move-capture? m) m)))
                     (when capture
                       (return-from matches (and (eq (move-captured-piece m) capture) m)))
                     (return-from matches m)))))))

      (labels ((peek (&optional (eof-error t))
                 (peek-char nil in eof-error nil))

               (next (&optional (eof-error t))
                 (read-char in eof-error nil))

               (read-piece ()
                 (let* ((ch (peek))
                        (p (char-piece ch)))
                   (when (and p (or (> (char-code ch) 255)
                                    (is-white? p)))
                     (next)
                     (logior side (logand p +PIECE+)))))

               (read-field ()
                 (let (file rank)
                   (setf file (peek nil))
                   (if (and file (char<= #\a file #\h))
                       (progn
                         (setf file (- (char-code file) 97))
                         (next))
                       (setf file nil))
                   (setf rank (peek nil))
                   (if (and rank (char<= #\1 rank #\8))
                       (progn
                         (setf rank (- (char-code rank) 49))
                         (next))
                       (setf rank nil))
                   (values file rank)))

               (read-from ()
                 (multiple-value-bind (file rank) (read-field)
                   (setf from-file file
                         from-rank rank)
                   (when (and file rank)
                     (setf from (board-index rank file)))))

               (maybe-skip (&rest chars)
                 (when (member (peek) chars :test #'char=)
                   (next)))

               (skip-sep ()
                 (awhen (maybe-skip #\x #\: #\-)
                   (when (or (char= it #\x) (char= it #\:))
                     (setf capture t))))

               (read-to ()
                 (multiple-value-bind (file rank) (read-field)
                   (setf to-file file
                         to-rank rank)
                   (when (and file rank)
                     (setf to (board-index rank file)))))

               (read-promo ()
                 (when (char= (peek) #\=)
                   (next))
                 (read-piece))

               (read-castle ()
                 (when (char-equal (peek) #\O)
                   (next)
                   (unless (char= (next) #\-)
                     (error "Invalid input"))
                   (unless (char-equal (next) #\O)
                     (error "Invalid input"))
                   (cond
                     ((eq (peek nil) #\-)
                      (next)
                      (unless (char= (next) #\O)
                        (error "Invalid input"))
                      (setf piece (logior +KING+ side)
                            from  (if white $E1 $E8)
                            to    (if white $C1 $C8)))
                     (t
                      (setf piece (logior +KING+ side)
                            from  (if white $E1 $E8)
                            to    (if white $G1 $G8))))
                   t)))

        (handler-case
            (unless (read-castle)
              (setf piece (read-piece))
              (read-from)
              (skip-sep)
              (when capture
                (awhen (read-piece)
                  (setf capture (logxor it +WHITE+))))
              (read-to)
              (setf promo (read-promo))
              (loop while (maybe-skip #\# #\+ #\! #\?))

              (unless piece
                (setf piece (logior side +PAWN+)))

              (when (and (or from-file from-rank)
                         (not to-file) (not to-rank)) ; only destination is specified
                (setf to from
                      from nil
                      to-file from-file
                      to-rank from-rank
                      from-file nil
                      from-rank nil)))
          (end-of-file ())))

      (loop for m in moves when (matches m) collect m))))

(defmethod game-parse-san ((game game) (san string)
                           &optional (moves (game-compute-moves game)))
  (with-input-from-string (in san)
    (game-parse-san game in moves)))

(defmethod game-san ((game game) move &key (moves (game-compute-moves game)) &allow-other-keys)
  (declare (type move move))
  (let ((piece (move-piece move))
        (from (move-from move))
        (to (move-to move)))
    (with-output-to-string (out)
      (with-row-col (from row col)
        (cond
          ((move-oo? move) (write-string "O-O" out))
          ((move-ooo? move) (write-string "O-O-O" out))
          ((is-pawn? piece)
           (when (move-capture? move)
             (format out "~Cx" (code-char (+ col 97))))
           (write-string (index-field to) out)
           (awhen (move-promoted-piece move)
             (format out "=~C" (char-upcase (piece-char it)))))
          (t
           (let (same-row same-col ambiguous)
             (loop for m in moves until (and same-row same-col) do
               (when (and (= (move-piece m) piece)
                          (/= (move-from m) from)
                          (= (move-to m) to))
                 (setf ambiguous t)
                 (when (= (index-row (move-from m)) row)
                   (setf same-row t))
                 (when (= (index-col (move-from m)) col)
                   (setf same-col t))))
             (write-char (char-upcase (piece-char piece)) out)
             (when ambiguous
               (cond
                 ((and same-col same-row)
                  (write-string (index-field from) out))
                 (same-col
                  (write-char (code-char (+ row 49)) out))
                 (t
                  (write-char (code-char (+ col 97)) out))))
             (when (move-capture? move)
               (write-string "x" out))
             (write-string (index-field to) out)))))
      (with-move (game move)
        (if (attacked? game)
            (write-string (if (null (game-compute-moves game))
                              "#"
                              "+")
                          out))))))

(defmethod draw-by-material? ((game game))
  (let ((has-knights nil)
        (has-bishops nil))
    (board-foreach
     (game-board game)
     (lambda (p row col)
       (cond
         ((not (zerop (logand p #.(logior +QUEEN+ +ROOK+ +PAWN+))))
          (return-from draw-by-material? nil))
         ((is-bishop? p)
          (when has-knights
            (return-from draw-by-material? nil))
          (let ((color (logand (+ row col) 1)))
            (when (and has-bishops (/= has-bishops color))
              (return-from draw-by-material? nil))
            (setf has-bishops color)))
         ((is-knight? p)
          (when has-bishops
            (return-from draw-by-material? nil))
          (setf has-knights t)))))
    t))

(defun perft (game depth)
  (let ((captures 0)
        (enpa 0)
        (castles 0)
        (promotions 0))
    (labels ((rec (depth)
               (if (zerop depth)
                   1
                   (loop for m in (game-compute-moves game)
                         when (and (= depth 1) (move-capture? m)) do (incf captures)
                           when (and (= depth 1) (move-enpa? m)) do (incf enpa)
                             when (and (= depth 1) (move-castle? m)) do (incf castles)
                               when (and (= depth 1) (move-promote? m)) do (incf promotions)
                                 summing (with-move (game m) (rec (1- depth)))))))
      (let ((count (rec depth)))
        (format t "Depth: ~D, Count: ~D, Captures: ~D, Enpa: ~D, Promo: ~D, Castle: ~D~%"
                depth count captures enpa promotions castles)
        (values count captures enpa castles promotions)))))

(defun divide (game depth)
  (loop with moves = (game-compute-moves game)
        with count and captures and enpa and castles and promotions
        for m in moves
        for san = (game-san game m :moves moves)
        do (with-move (game m)
             (with-output-to-string (*standard-output*)
               (multiple-value-setq (count captures enpa castles promotions) (perft game (1- depth))))
             (format t "~A~A ~A~%"
                     (index-field (move-from m))
                     (index-field (move-to m))
                     count))))



;;; XXX: test code

;; (defun print-move (move)
;;   (declare (type move move))
;;   (cond
;;     ((move-oo? move)
;;      "O-O")
;;     ((move-ooo? move)
;;      "O-O-O")
;;     (t
;;      (format nil "[~A]~A~A~A~A"
;;              (piece-char (move-piece move))
;;              (index-field (move-from move))
;;              (aif (move-captured-piece move)
;;                   (format nil "~A[~A]"
;;                           (if (move-enpa? move) #\/ #\x)
;;                           (piece-char it))
;;                   #\-)
;;              (index-field (move-to move))
;;              (aif (move-promoted-piece move)
;;                   (format nil "=~A" (char-upcase (piece-char it)))
;;                   "")))))

;; (defparameter g (make-instance 'game))
;; (reset-game g)

;; ;; (defun m (game from to &optional (promo 0))
;; ;;   (let* ((board (game-board game))
;; ;;          (piece (board-get board from))
;; ;;          (capture (board-get board to))
;; ;;          (oo (and (is-king? piece)
;; ;;                   (= to (+ from 2))))
;; ;;          (ooo (and (is-king? piece)
;; ;;                    (= to (- from 2))))
;; ;;          (enpa (and (is-pawn? piece)
;; ;;                     (logtest 1 (abs (- from to)))
;; ;;                     (zerop capture))))
;; ;;     (when enpa
;; ;;       (setf capture (aref board
;; ;;                           (with-row-col (from row col) row)
;; ;;                           (with-row-col (to row col) col))))
;; ;;     (make-move from to piece :enpa (if enpa 1 0)
;; ;;                              :oo (if oo 1 0)
;; ;;                              :ooo (if ooo 1 0)
;; ;;                              :capture capture
;; ;;                              :promo promo)))

;; (defparameter mqueue nil)

;; (defun pg (&optional (game g))
;;   (print-board (game-board game))
;;   (format t "State: ~4,'0b (~a), Side: ~a, Enpa: ~a (~a)"
;;           (game-state game) (game-state game)
;;           (game-side game)
;;           (and (game-enpa game)
;;                (index-field (game-enpa game))) (game-enpa game)))

;; (defun m (&rest args)
;;   (let* ((moves (apply #'game-parse-san g args))
;;          (move (car moves)))
;;     (when (> (length moves) 1)
;;       (format t "Ambiguous: ~{~A ~}" (mapcar #'(lambda (m)
;;                                                  (game-san g m)) moves))
;;       (return-from m))
;;     (when (zerop (length moves))
;;       (format t "Invalid move")
;;       (return-from m))
;;     (format t "> ~A~%" (game-san g move))
;;     (push (list move (game-state g) (game-enpa g)) mqueue)
;;     (game-move g move)
;;     (pg)
;;     move))

;; (defun undo ()
;;   (destructuring-bind (move state enpa) (pop mqueue)
;;     (game-undo-move g move)
;;     (setf (game-state g) state
;;           (game-enpa g) enpa))
;;   (pg))
