(in-package #:queen)

(in-readtable queen::syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +QUEEN+      #x01)
  (defconstant +ROOK+       #x02)
  (defconstant +KNIGHT+     #x04)
  (defconstant +BISHOP+     #x08)
  (defconstant +PAWN+       #x10)
  (defconstant +KING+       #x20)

  (defconstant +WQUEEN+     (logior #x01 #x40))
  (defconstant +WROOK+      (logior #x02 #x40))
  (defconstant +WKNIGHT+    (logior #x04 #x40))
  (defconstant +WBISHOP+    (logior #x08 #x40))
  (defconstant +WPAWN+      (logior #x10 #x40))
  (defconstant +WKING+      (logior #x20 #x40))

  (defconstant +PROMOTABLE+ #x0f)
  (defconstant +CAPTURABLE+ #x3f)
  (defconstant +PIECE+      #x3f)

  (defconstant +WHITE+      #x40)
  (defconstant +BLACK+      #x00))

(defparameter +FEN-START+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defparameter *unicode* nil)

(declaim (inline piece
                 piece-side
                 index-valid?
                 white
                 board-index
                 field-index
                 index-field
                 index-row
                 index-col
                 is-pawn?
                 is-knight?
                 is-bishop?
                 is-rook?
                 is-queen?
                 is-king?
                 is-black?
                 is-white?
                 same-side?
                 opp-side?
                 board-get
                 board-get-rc
                 board-set
                 board-set-rc
                 make-move
                 move-from
                 move-to
                 move-piece
                 move-white?
                 move-black?
                 move-side
                 move-capture?
                 move-captured-piece
                 move-promote?
                 move-promoted-piece
                 move-set-promoted-piece
                 move-check?
                 move-set-check
                 move-enpa?
                 move-captured-index
                 move-oo?
                 move-ooo?
                 move-castle?))

(deftype piece ()
  '(unsigned-byte 8))

(deftype board-index ()
  '(integer 0 119))

(deftype board ()
  '(simple-array piece (120)))

(defun piece (p)
  (declare (type piece p))
  (logand p +PIECE+))

(defun piece-side (p)
  (declare (type piece p))
  (logand p +WHITE+))

(defun index-valid? (index)
  (declare (optimize speed)
           (type fixnum index))
  (and (typep index 'board-index)
       (not (logtest index #x88))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun board-index (row col)
    (declare (optimize speed)
             (type (integer 0 7) row col))
    (dpb row (byte 3 4) col))

  (defun field-index (field)
    (declare (type (simple-string 2) field))
    (let ((col (aref field 0))
          (row (aref field 1)))
      (board-index (- (char-code row) 49)
                   (- (logior 32 (char-code col)) 97))))

  (set-macro-character
   #\$ (lambda (stream ch)
         (declare (ignore ch))
         (field-index (coerce (list (read-char stream)
                                    (read-char stream)) 'string)))))

(defun index-row (index)
  (declare (type board-index index))
  (ldb (byte 3 4) index))

(defun index-col (index)
  (declare (type board-index index))
  (ldb (byte 3 0) index))

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
        (#.+WPAWN+ #\P)
        (#.+WKNIGHT+ #\N)
        (#.+WKING+ #\K)
        (#.+WBISHOP+ #\B)
        (#.+WROOK+ #\R)
        (#.+WQUEEN+ #\Q))))

(defun piece-unicode (p)
  (declare (type piece p))
  (ecase p
    ;; empty
    (0 #\□)
    ;; black
    (#.+PAWN+ #\♟)
    (#.+KNIGHT+ #\♞)
    (#.+KING+ #\♚)
    (#.+BISHOP+ #\♝)
    (#.+ROOK+ #\♜)
    (#.+QUEEN+ #\♛)
    ;; white
    (#.+WPAWN+ #\♙)
    (#.+WKNIGHT+ #\♘)
    (#.+WKING+ #\♔)
    (#.+WBISHOP+ #\♗)
    (#.+WROOK+ #\♖)
    (#.+WQUEEN+ #\♕)))

(defun char-piece (p)
  (declare (type character p))
  (case p
    (#\- 0)
    ;; black
    ((#\p #\♟) +PAWN+)
    ((#\n #\♞) +KNIGHT+)
    ((#\k #\♚) +KING+)
    ((#\b #\♝) +BISHOP+)
    ((#\r #\♜) +ROOK+)
    ((#\q #\♛) +QUEEN+)
    ;; white
    ((#\P #\♙) #.+WPAWN+)
    ((#\N #\♘) #.+WKNIGHT+)
    ((#\K #\♔) #.+WKING+)
    ((#\B #\♗) #.+WBISHOP+)
    ((#\R #\♖) #.+WROOK+)
    ((#\Q #\♕) #.+WQUEEN+)))

(defun is-pawn? (p)
  (declare (type piece p))
  (logtest p +PAWN+))

(defun is-knight? (p)
  (declare (type piece p))
  (logtest p +KNIGHT+))

(defun is-bishop? (p)
  (declare (type piece p))
  (logtest p +BISHOP+))

(defun is-rook? (p)
  (declare (type piece p))
  (logtest p +ROOK+))

(defun is-queen? (p)
  (declare (type piece p))
  (logtest p +QUEEN+))

(defun is-king? (p)
  (declare (type piece p))
  (logtest p +KING+))

(defun is-white? (p)
  (declare (type piece p))
  (logtest p +WHITE+))

(defun is-black? (p)
  (declare (type piece p))
  (not (is-white? p)))

(defun same-side? (p1 p2)
  (declare (type piece p1 p2))
  (= (logand p1 +WHITE+) (logand p2 +WHITE+)))

(defun opp-side? (p1 p2)
  (declare (type piece p1 p2))
  (not (same-side? p1 p2)))

(defun board-get-rc (board row col)
  (declare (optimize speed)
           (type board board)
           (type (integer 0 7) row col))
  (aref board (board-index row col)))

(defun board-set-rc (board row col piece)
  (declare (optimize speed)
           (type board board)
           (type (integer 0 7) row col)
           (type piece piece))
  (setf (aref board (board-index row col)) piece))

(defun board-get (board index)
  (declare (optimize speed)
           (type board board)
           (type board-index index))
  (aref board index))

(defun board-set (board index val)
  (declare (optimize speed)
           (type board board)
           (type board-index index)
           (type piece val))
  (setf (aref board index) val))

(defsetf board-get board-set)

(defmacro with-piece ((board pos p &optional allow-empty) &body body)
  (once-only (board pos)
    `(let ((,p (board-get ,board ,pos)))
       ,(if allow-empty
            `(progn ,@body)
            `(unless (zerop ,p)
               ,@body)))))

(defun make-board ()
  (make-array 120 :element-type 'piece :initial-element 0))

(defun board-foreach (board fn)
  (declare (optimize speed)
           (type board board)
           (type (function (piece (integer 0 7) (integer 0 7) board-index) t) fn))
  (loop for row from 0 to 7 do
    (loop for col from 0 to 7
          for index = (board-index row col)
          for piece = (board-get board index)
          when (not (zerop piece))
            do (funcall fn piece row col index))))

(defun print-board (board &key (output t))
  (loop for row from 7 downto 0
        for line = (loop for col from 0 to 7
                         collect (piece-char (board-get-rc board row col)))
        do (format output "~D │ ~{~A~^ ~}~%" (+ row 1) line))
  (format output "  └─────────────────~%")
  (format output "    a b c d e f g h~%"))

(defconstant +WHITE-OO+     1)
(defconstant +WHITE-OOO+    2)
(defconstant +WHITE-CASTLE+ 3)
(defconstant +BLACK-OO+     4)
(defconstant +BLACK-OOO+    8)
(defconstant +BLACK-CASTLE+ 12)
(defconstant +ALL-CASTLE+   15)

(defstruct game
  (board (make-board) :type board)
  (state 0 :type (unsigned-byte 32))
  (side +WHITE+ :type piece)
  (enpa nil :type (or board-index null))
  (fullmove 0 :type (unsigned-byte 32))
  (halfmove 0 :type (unsigned-byte 32)))

(defmethod reset-from-fen ((game game) (in stream))
  (let ((board (game-board game))
        (state 0))
    (with-parse-stream in
      (labels ((read-row (row)
                 (loop for ch = (next)
                       for col upfrom 0
                       do (cond
                            ((find ch "pnkbrqPNKBRQ")
                             (board-set-rc board row col (char-piece ch)))
                            ((find ch "12345678")
                             (dotimes (i (- (char-code ch) 48))
                               (board-set-rc board row col 0)
                               (incf col))
                             (decf col))
                            (t (unget ch)
                               (return)))))

               (read-position ()
                 (loop for row from 7 downto 0
                       do (read-row row)
                          (unless (zerop row)
                            (skip #\/))))

               (read-side ()
                 (case (next)
                   (#\w (setf (game-side game) +WHITE+))
                   (#\b (setf (game-side game) +BLACK+))
                   (otherwise (error "Cannot read playing side"))))

               (read-castling ()
                 (if (eql (peek) #\-)
                     (next)
                     (loop while (find (peek) "kqKQ")
                           do (case (next)
                                (#\k (setf state (logior state +BLACK-OO+)))
                                (#\q (setf state (logior state +BLACK-OOO+)))
                                (#\K (setf state (logior state +WHITE-OO+)))
                                (#\Q (setf state (logior state +WHITE-OOO+))))))
                 (setf (game-state game) state))

               (read-en-passant ()
                 (if (eql (peek) #\-)
                     (progn
                       (next)
                       (setf (game-enpa game) nil))
                     (let ((col (next))
                           (row (next)))
                       (unless (and (find col "abcdefghABCDEFGH")
                                    (find row "12345678"))
                         (error "Invalid en-passant field"))
                       (setf (game-enpa game)
                             (board-index (- (char-code row) 49)
                                          (- (char-code (char-downcase col)) 97))))))

               (read-halfmove ()
                 (setf (game-halfmove game) (read-number)))

               (read-fullmove ()
                 (setf (game-fullmove game) (read-number))))

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
        game))))

(defmethod reset-from-fen ((game game) (fen string))
  (with-input-from-string (in fen)
    (reset-from-fen game in)))

(defmethod reset-game ((game game))
  (reset-from-fen game +FEN-START+))

(defmethod game-fen ((game game))
  (let ((board (game-board game))
        (state (game-state game))
        (enpa (game-enpa game))
        (*unicode* nil))
    (with-output-to-string (*standard-output*)
      (loop for row from 7 downto 0
            do (loop with empty = 0
                     for col from 0 to 7
                     for p = (board-get-rc board row col)
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

(defun make-move (from to piece capture enpa)
  (declare (optimize speed)
           (type board-index from to)
           (type piece piece capture)
           (type (unsigned-byte 1) enpa))
  (let  ((move (dpb (index-col from) (byte 3 0) 0)))
    (declare (type move move))
    (setf move (dpb (index-row from) (byte 3 3) move))
    (setf move (dpb (index-col to) (byte 3 6) move))
    (setf move (dpb (index-row to) (byte 3 9) move))
    (setf move (dpb piece (byte 7 12) move))
    (setf move (dpb capture (byte 6 23) move))
    (setf move (dpb enpa (byte 1 29) move))))

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

(defun move-white? (move)
  (declare (type move move))
  (ldb-test (byte 1 18) move))

(defun move-black? (move)
  (declare (type move move))
  (not (move-white? move)))

(defun move-side (move)
  (declare (type move move))
  (ash (ldb (byte 1 18) move) 6))

(defun move-capture? (move)
  (declare (type move move))
  (ldb-test (byte 6 23) move))

(defun move-captured-piece (move)
  (declare (type move move))
  (let ((p (ldb (byte 6 23) move)))
    (cond
      ((zerop p) nil)
      ((move-black? move) (logior p +WHITE+))
      (t p))))

(defun move-promote? (move)
  (declare (type move move))
  (ldb-test (byte 4 19) move))

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
  (dpb promo (byte 4 19) move))

(defun move-set-check (move)
  (declare (type move move))
  (dpb 1 (byte 1 30) move))

(defun move-check? (move)
  (declare (type move move))
  (ldb-test (byte 1 30) move))

(defun move-enpa? (move)
  (declare (type move move))
  (ldb-test (byte 1 29) move))

(defun move-captured-index (move)
  (declare (type move move))
  (cond
    ((move-enpa? move)
     (board-index (ldb (byte 3 3) move)
                  (ldb (byte 3 6) move)))
    ((move-capture? move)
     (move-to move))
    (t
     (error "Not a capturing move"))))

(defun move-oo? (move)
  (declare (type move move))
  (= (logand move #b0111111000111000111)
     #||#         #b0100000000110000100))

(defun move-ooo? (move)
  (declare (type move move))
  (= (logand move #b0111111000111000111)
     #||#         #b0100000000010000100))

(defun move-castle? (move)
  (declare (type move move))
  (= (logand move #b0111111000011000111)
     #||#         #b0100000000010000100))

;;; move execution

(defun game-move (game move &optional quick)
  (declare (optimize speed)
           (type game game)
           (type move move))
  (let ((board (game-board game))
        (from (move-from move))
        (to (move-to move))
        (piece (move-piece move))
        (white (move-white? move))
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
          (board-set board $F1 +WROOK+))
         (t
          (board-set board $H8 0)
          (board-set board $F8 +ROOK+))))
      ((move-ooo? move)
       (cond
         (white
          (board-set board $A1 0)
          (board-set board $D1 +WROOK+))
         (t
          (board-set board $A8 0)
          (board-set board $D8 +ROOK+))))
      ((move-enpa? move)
       (board-set board (move-captured-index move) 0)))

    ;; update side to move and en-passant target
    (setf (game-side game) (if white +BLACK+ +WHITE+)
          (game-enpa game) (when (and (is-pawn? piece)
                                      (= (abs (- from to)) 32))
                             (ash (+ from to) -1)))

    ;; update castling state
    (symbol-macrolet ((state (game-state game)))
      (when (and (logtest state +WHITE-OO+)
                 (or (= from $E1) (= from $H1) (= to $H1)))
        (setf state (logxor state +WHITE-OO+)))
      (when (and (logtest state +WHITE-OOO+)
                 (or (= from $E1) (= from $A1) (= to $A1)))
        (setf state (logxor state +WHITE-OOO+)))
      (when (and (logtest state +BLACK-OO+)
                 (or (= from $E8) (= from $H8) (= to $H8)))
        (setf state (logxor state +BLACK-OO+)))
      (when (and (logtest state +BLACK-OOO+)
                 (or (= from $E8) (= from $A8) (= to $A8)))
        (setf state (logxor state +BLACK-OOO+))))

    ;; fullmove and halfmove counters
    (unless quick
      (unless white
        (incf (game-fullmove game)))
      (if (or (is-pawn? piece) (move-capture? move))
          (setf (game-halfmove game) 0)
          (incf (game-halfmove game))))))

(defun board-undo-move (board move)
  (declare (optimize speed)
           (type board board)
           (type move move))
  (let ((from (move-from move))
        (to (move-to move))
        (captured (move-captured-piece move))
        (piece (move-piece move)))
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
         ((move-white? move)
          (board-set board $H1 +WROOK+)
          (board-set board $F1 0))
         (t
          (board-set board $H8 +ROOK+)
          (board-set board $F8 0))))
      ((move-ooo? move)
       (cond
         ((move-white? move)
          (board-set board $A1 +WROOK+)
          (board-set board $D1 0))
         (t
          (board-set board $A8 +ROOK+)
          (board-set board $D8 0)))))))

(defmacro with-move ((game move &optional quick) &body body)
  (once-only (game move)
    (with-gensyms (state enpa halfmove fullmove side)
      `(let ((,state (game-state ,game))
             (,enpa (game-enpa ,game))
             (,side (game-side ,game))
             ,@(unless quick
                 `((,halfmove (game-halfmove ,game))
                   (,fullmove (game-fullmove ,game)))))
         (game-move ,game ,move ,quick)
         (prog1
             (progn ,@body)
           (board-undo-move (game-board ,game) ,move)
           (setf (game-side ,game) ,side
                 (game-state ,game) ,state
                 (game-enpa ,game) ,enpa
                 ,@(unless quick
                     `((game-halfmove ,game) ,halfmove
                       (game-fullmove ,game) ,fullmove))))))))

;;; move generation

(defparameter +MOVES-KNIGHT+ '(31 33 14 18 -18 -14 -33 -31))
(defparameter +MOVES-BISHOP+ '(15 17 -15 -17))
(defparameter +MOVES-ROOK+   '(1 16 -16 -1))
(defparameter +MOVES-QING+   `(,@+MOVES-BISHOP+ ,@+MOVES-ROOK+))

(defun king-index (game &optional (side (game-side game)))
  (declare (optimize speed)
           (type game game)
           (type piece side))
  (loop with king = (logior +KING+ side)
        with board = (game-board game)
        for row from 0 to 7 do
          (loop for col from 0 to 7
                for index = (board-index row col)
                when (= (board-get board index) king)
                  do (return-from king-index index))))

(defun attacked? (game &optional
                         (side (game-side game))
                         (index (king-index game side)))
  (declare (optimize speed)
           (type piece side)
           (type game game)
           (type board-index index))
  (let* ((board (game-board game))
         (opp (logxor side +WHITE+)))
    (labels ((test (p piece)
               (when (and (same-side? p opp)
                          (= p (logand p piece)))
                 (return-from attacked? t)))
             (check (piece delta)
               (declare (type piece piece)
                        (type fixnum delta))
               (let ((pos (+ index delta)))
                 (when (index-valid? pos)
                   (with-piece (board pos p)
                     (test p piece)))))
             (repeat (piece delta)
               (declare (type fixnum delta))
               (loop for pos fixnum = (+ index delta) then (+ pos delta)
                     while (index-valid? pos)
                     do (with-piece (board pos p)
                          (test p piece)
                          (return)))))
      (declare (inline test check repeat))
      (cond ((is-white? opp)
             (check +WPAWN+ -15)
             (check +WPAWN+ -17))
            (t
             (check +PAWN+ +15)
             (check +PAWN+ +17)))
      (loop with piece = (logior opp +KNIGHT+)
            for delta in +MOVES-KNIGHT+
            do (check piece delta))
      (loop with piece = (logior opp +BISHOP+ +QUEEN+)
            for delta in +MOVES-BISHOP+
            do (repeat piece delta))
      (loop with piece = (logior opp +ROOK+ +QUEEN+)
            for delta in +MOVES-ROOK+
            do (repeat piece delta))
      (loop with piece = (logior opp +KING+)
            for delta in +MOVES-QING+
            do (check piece delta))
      nil)))

(defun game-compute-moves (game)
  (declare (optimize speed)
           (type game game))
  (let* ((side (game-side game))
         (opp (logxor side +WHITE+))
         (board (game-board game))
         (moves '())
         (enpa (game-enpa game))
         (my-king (king-index game side))
         (opp-king (king-index game opp))
         flag in-check)
    (loop for row fixnum from 0 to 7 do
      (loop for col fixnum from 0 to 7
            for from = (board-index row col)
            for piece = (board-get board from)
            when (same-side? piece side) do
              (labels
                  ((move-pawn (c1 c2 a1 a2 on-start on-end)
                     (labels ((try-enpa (delta)
                                (when enpa
                                  (let ((to (+ from delta)))
                                    (when (= to enpa)
                                      (add (make-move from to piece (logior +PAWN+ opp) 1))))))
                              (try-capture (delta)
                                (let ((to (+ from delta)))
                                  (when (index-valid? to)
                                    (with-piece (board to p)
                                      (when (opp-side? piece p)
                                        (maybe-promote (make-move from to piece p 0)))))))
                              (try-advance (delta)
                                (let ((to (+ from delta)))
                                  ;; `to' index should be always valid
                                  (with-piece (board to p t)
                                    (when (zerop p)
                                      (maybe-promote (make-move from to piece 0 0))
                                      ;; we want to return true if the field is empty, so that
                                      ;; i.e. if we're in check and D3 doesn't get us out (in which
                                      ;; case `add' will return nil), we still want to try D4.
                                      t))))
                              (maybe-promote (move)
                                (cond
                                  (on-end
                                   (when (add (move-set-promoted-piece move +KNIGHT+))
                                     (%add (move-set-promoted-piece move +BISHOP+))
                                     (%add (move-set-promoted-piece move +ROOK+))
                                     (%add (move-set-promoted-piece move +QUEEN+))))
                                  (t
                                   (add move)))))

                       (declare (inline try-enpa try-capture try-advance maybe-promote))

                       (unless (try-enpa c1) (try-capture c1))
                       (unless (try-enpa c2) (try-capture c2))
                       (when (and (try-advance a1) on-start)
                         (try-advance a2))))

                   (move-knight ()
                     (mapc #'move +MOVES-KNIGHT+))

                   (move-bishop ()
                     (mapc #'repeat +MOVES-BISHOP+))

                   (move-rook ()
                     (mapc #'repeat +MOVES-ROOK+))

                   (move-queen ()
                     (mapc #'repeat +MOVES-QING+))

                   (in-check? ()
                     (if flag
                         in-check
                         (setf flag t
                               in-check (attacked? game side my-king))))

                   (move-king (oo ooo oo1 oo2 ooo1 ooo2 ooo3)
                     (mapc #'move +MOVES-QING+)
                     ;; note: `add' discards all moves that leave our king in check, so it's not
                     ;; necessary to test here whether the target field is attacked; we only
                     ;; have to check the middle field (i.e. F1 for white's O-O).
                     (when (and (logtest (game-state game) oo)
                                (zerop (board-get board oo1))
                                (zerop (board-get board oo2))
                                (not (in-check?))
                                (not (attacked? game side oo1)))
                       (add (make-move from oo2 piece 0 0)))
                     (when (and (logtest (game-state game) ooo)
                                (zerop (board-get board ooo1))
                                (zerop (board-get board ooo2))
                                (zerop (board-get board ooo3))
                                (not (in-check?))
                                (not (attacked? game side ooo1)))
                       (add (make-move from ooo2 piece 0 0))))

                   (move (delta)
                     (declare (type fixnum delta))
                     (let ((to (+ from delta)))
                       (when (index-valid? to)
                         (with-piece (board to p t)
                           (when (or (zerop p) (opp-side? p side))
                             (add (make-move from to piece p 0)))))))

                   (repeat (delta)
                     (declare (type fixnum delta))
                     (loop for to fixnum = (+ from delta) then (+ to delta)
                           while (index-valid? to) do
                             (let ((p (board-get board to)))
                               (cond
                                 ((zerop p)
                                  (add (make-move from to piece 0 0)))
                                 ((opp-side? p side)
                                  (add (make-move from to piece p 0))
                                  (return))
                                 (t
                                  (return))))))

                   (add (m)
                     (with-move (game m t)
                       (let ((index (if (is-king? piece) (move-to m) my-king)))
                         (unless (attacked? game side index)
                           (car (push (if (attacked? game opp opp-king)
                                          (move-set-check m)
                                          m)
                                      moves))))))

                   (%add (m)
                     (with-move (game m t)
                       (car (push (if (attacked? game opp opp-king)
                                      (move-set-check m)
                                      m)
                                  moves)))))

                (declare (inline move-pawn move-king in-check?))

                (case piece
                  (#.+PAWN+                  (move-pawn -15 -17 -16 -32 (= row 6) (= row 1)))
                  (#.+WPAWN+                 (move-pawn +15 +17 +16 +32 (= row 1) (= row 6)))
                  ((#.+KNIGHT+ #.+WKNIGHT+)  (move-knight))
                  ((#.+BISHOP+ #.+WBISHOP+)  (move-bishop))
                  ((#.+ROOK+ #.+WROOK+)      (move-rook))
                  ((#.+QUEEN+ #.+WQUEEN+)    (move-queen))
                  (#.+KING+                  (move-king +BLACK-OO+ +BLACK-OOO+ $F8 $G8 $D8 $C8 $B8))
                  (#.+WKING+                 (move-king +WHITE-OO+ +WHITE-OOO+ $F1 $G1 $D1 $C1 $B1))))))

    moves))

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
                              (eql promo (move-promoted-piece m)))
                     m)))
               (when (and (eql piece (move-piece m))
                          (eql promo (move-promoted-piece m)))
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
                     (when (eql capture t)
                       (return-from matches (and (move-capture? m) m)))
                     (when capture
                       (return-from matches (and (eql (move-captured-piece m) capture) m)))
                     (return-from matches m)))))))

      (with-parse-stream in
        (labels ((read-piece ()
                   (let* ((ch (peek))
                          (p (and ch (char-piece ch))))
                     (when (and p (or (> (char-code ch) 255)
                                      (is-white? p)))
                       (next)
                       (logior side (piece p)))))

                 (read-field ()
                   (let (file rank)
                     (setf file (peek))
                     (if (and file (char<= #\a file #\h))
                         (progn
                           (setf file (- (char-code file) 97))
                           (next))
                         (setf file nil))
                     (setf rank (peek))
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
                   (when (member (peek) chars :test #'eql)
                     (next)))

                 (skip-sep ()
                   (awhen (maybe-skip #\x #\: #\-)
                     (when (or (eql it #\x) (eql it #\:))
                       (setf capture t))))

                 (read-to ()
                   (multiple-value-bind (file rank) (read-field)
                     (setf to-file file
                           to-rank rank)
                     (when (and file rank)
                       (setf to (board-index rank file)))))

                 (read-promo ()
                   (when (eql (peek) #\=)
                     (next))
                   (read-piece))

                 (read-castle ()
                   (when (eql (peek) #\O)
                     (next)
                     (unless (eql (next) #\-)
                       (return-from game-parse-san nil))
                     (unless (eql (next) #\O)
                       (return-from game-parse-san nil))
                     (cond
                       ((eql (peek) #\-)
                        (next)
                        (unless (eql (next) #\O)
                          (return-from game-parse-san nil))
                        (setf piece (logior +KING+ side)
                              from  (if white $E1 $E8)
                              to    (if white $C1 $C8)))
                       (t
                        (setf piece (logior +KING+ side)
                              from  (if white $E1 $E8)
                              to    (if white $G1 $G8))))
                     t)))

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

            (when (and (not piece) (or from-file from-rank to-file to-rank))
              (setf piece (logior side +PAWN+)))

            (when (and (or from-file from-rank)
                       (not to-file) (not to-rank)) ; only destination is specified
              (setf to from
                    from nil
                    to-file from-file
                    to-rank from-rank
                    from-file nil
                    from-rank nil)))))

      (loop for m in moves when (matches m) collect m))))

(defmethod game-parse-san ((game game) (san string)
                           &optional (moves (game-compute-moves game)))
  (with-input-from-string (in san)
    (game-parse-san game in moves)))

(defmethod game-san ((game game) move &optional (moves (game-compute-moves game)))
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
      (with-move (game move t)
        (if (attacked? game)
            (write-string (if (null (game-compute-moves game))
                              "#"
                              "+")
                          out))))))

(defun draw-by-material? (game)
  (declare (optimize speed)
           (type game game))
  (let ((has-knights nil)
        (has-bishops nil))
    (board-foreach
     (game-board game)
     (lambda (p row col index)
       (declare (type (integer 0 7) row col)
                (type board-index index)
                (ignore index))
       (cond
         ((logtest p #.(logior +QUEEN+ +ROOK+ +PAWN+))
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

;; EOF - TEST STUFF

(defun perft (game depth &optional count-mates)
  (let ((captures 0)
        (enpa 0)
        (castles 0)
        (promotions 0)
        (checks 0)
        (count 0)
        (checkmates 0))
    (labels ((rec (depth)
               (declare (type (unsigned-byte 8) depth))
               (let ((moves (game-compute-moves game)))
                 (if (> depth 1)
                     (loop for m in moves do
                       (with-move (game m t)
                         (rec (1- depth))))
                     (loop for m in moves
                           when (move-capture? m)
                             do (incf captures)
                           when (move-enpa? m)
                             do (incf enpa)
                           when (move-castle? m)
                             do (incf castles)
                           when (move-promote? m)
                             do (incf promotions)
                           when (move-check? m)
                             do (incf checks)
                           do (incf count)
                              (when (and count-mates (move-check? m))
                                (with-move (game m t)
                                  (if (and (attacked? game)
                                           (null (game-compute-moves game)))
                                      (incf checkmates)))))))))
      (rec depth)
      (format t "Depth: ~D, Count: ~D, Captures: ~D, Enpa: ~D, Checks: ~D, Promo: ~D, Castle: ~D~%"
              depth count captures enpa checks promotions castles)
      (values count captures enpa castles promotions checks checkmates))))

(defun divide (game depth)
  (loop with moves = (game-compute-moves game)
        with count and captures and enpa and castles and promotions
        for m in moves
        for san = (game-san game m moves)
        do (with-move (game m t)
             (with-output-to-string (*standard-output*)
               (multiple-value-setq (count captures enpa castles promotions) (perft game (1- depth))))
             (format t "~A~A ~A~%"
                     (index-field (move-from m))
                     (index-field (move-to m))
                     count))))
