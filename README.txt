QUEEN - Chess Utilities For Common Lisp
=======================================


What's in it:

- board representation (0x88 method)
- move generation (passes perft tests)
- FEN/SAN/PGN parser and generator
- basic (weak) evaluation engine (still, it kinda beats me at depth=5)

This is only tested on SBCL, but there is no SBCL-specific code, so it
should work on other implementations.  I'd love to hear about it.


Synopsis
--------

    ;; create game
    (setf g (make-game))
    (reset-game g)

    ;; print the board with RNBQKP notation
    (print-board (game-board g))

    ;; print board with Unicode chars for pieces
    (let ((*unicode* t)) (print-board (game-board g)))

    ;; generate available moves
    (setf moves (game-compute-moves g))

    ;; print moves
    (loop for m in moves
          do (format t "~A~%" (game-san g m)))

    ;; perft test to depth 6 (~36 sec. on my laptop from start position)
    (time (perft g 6))

    ;; try a move
    (with-move (g (car (game-parse-san g "Nf3")))
      (print-board (game-board g)))        ; prints board after Nf3
    (print-board (game-board g))           ; back to previous position

    ;; execute a move
    (game-move g (car (game-parse-san g "d4")))
    (print-board (game-board g))

    ;; is the current side in check?
    (attacked? g)

    ;; find best line to depth 5 (default: +MAX-DEPTH+)
    (setf line (game-search g 5))
    (cond
      ((and (null line) (attacked? g))
       (format t "Checkmate~%"))
      ((null line)
       (format t "Draw~%"))
      (t
       (format t "~A~%" (dump-line g line))))

    ;; initialize from FEN
    (reset-from-fen g "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10")
    (time (perft g 5))
    ;; -->
    ;; Depth: 5, Count: 164075551, Captures: 19528068, Enpa: 122, Checks: 2998608, Promo: 0, Castle: 0
    ;; Evaluation took:
    ;;  54.679 seconds of real time

    ;; find mate in 3
    QUEEN> (reset-from-fen g "r4r2/1pNP4/pPk2N1Q/2BR1n2/1P6/8/2p2K1p/8 w - - 0 1")

    QUEEN> (let ((*unicode* t)) (print-board (game-board g)))
    8 │ ♜ □ □ □ □ ♜ □ □
    7 │ □ ♟ ♘ ♙ □ □ □ □
    6 │ ♟ ♙ ♚ □ □ ♘ □ ♕
    5 │ □ □ ♗ ♖ □ ♞ □ □
    4 │ □ ♙ □ □ □ □ □ □
    3 │ □ □ □ □ □ □ □ □
    2 │ □ □ ♟ □ □ ♔ □ ♟
    1 │ □ □ □ □ □ □ □ □
      └─────────────────
        a b c d e f g h

    QUEEN> (let ((*unicode* t)) (dump-line g (game-search g 5)))
    "1. ♔e1 h1=♛+ 2. ♕xh1 c1=♛+ 3. ♖d1#"

    ;; test the move generator using perftsuite.epd
    (run-perft-tests 5)         ; takes 2 minutes for depth 5

    ;; play a game against the engine
    (play &key fen depth)       ; default depth=5 and start position
    ;;
    ;; enter: valid move in SAN notation, or:
    ;;
    ;;        go -- force computer to move
    ;;        exit -- stop game
    ;;        restart -- restart game to given fen
    ;;        reset -- start new game
    ;;        undo -- undo last move
    ;;        pgn -- show all moves played so far in PGN
    ;;        fen -- show current position as FEN


Data structures and types
-------------------------

Note: this section documents internals, which might change.

- `piece` -- an `(unsigned-byte 8)`.  We use six bits for the piece and one for
  the side, as follows:

    ┌─┬─┬─┬─┬─┬─┬─┬─┐
    │7│6│5│4│3│2│1│0│
    └─┴┬┴┬┴┬┴┬┴┬┴┬┴┬┘
       │ │ │ │ │ │ └────── Queen
       │ │ │ │ │ └──────── Rook
       │ │ │ │ └────────── Knight
       │ │ │ └──────────── Bishop
       │ │ └────────────── Pawn
       │ └──────────────── King
       └────────────────── White

  A piece cannot be both a queen and a rook, of course — so we could have
  used fewer bits — but it's sometimes useful to test whether a piece is
  *either* a queen or a rook; by allocating one bit for each piece type, we
  can do that by AND-ing with 3 (Queen + Rook).

  The idea with the seemingly arbitrary ordering was to be able to represent
  pieces that we can promote to in 4 bits (Q, R, N, B), and pieces that can
  be captured in 5 (Q, R, N, B, P) — so the king is last.

- `board` -- a simple-array of 120 `piece` elements (of which only 64 are used).
  A zero element means the field is empty.

- `board-index` -- an integer between 0 and 119 (index in the `board` array).
  An integer value is a valid index if by AND-ing it with 0x88 we get zero
  (i.e. on other values we never store pieces, even if they still fall between 0
  and 119).

- `move` -- an `(unsigned-byte 32)`:

     3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
     1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
    │ │+│E│x│x│x│x│x│x│=│=│=│=│p│p│p│p│p│p│p│R│R│R│C│C│C│R│R│R│C│C│C│
    └─┴┬┴┬┴┬┴─┴─┴─┴─┴┬┴┬┴─┴─┴┬┴┬┴─┴─┴─┴─┴─┴┬┴┬┴─┴─┴─┴─┴┬┴┬┴─┴─┴─┴─┴┬┘
       │ │ └────┬────┘ └──┬──┘ └─────┬─────┘ └────┬────┘ └────┬────┘
       │ │      │         │          │            │           │
       │ │      │         │          │            │           └────── FROM
       │ │      │         │          │            └────────────────── TO
       │ │      │         │          └─────────────────────────────── PIECE
       │ │      │         └────────────────────────────────────────── PROMOTION
       │ │      └──────────────────────────────────────────────────── CAPTURE
       │ └─────────────────────────────────────────────────────────── ENPA
       └───────────────────────────────────────────────────────────── CHECK

     R : row, 3 bits (integer 0 7)
     C : col, 3 bits (integer 0 7)
     p : piece moved, 7 bits (including side: bit 18 is 1 for white moves)
     = : piece promoted to, 4 bits (same side as p)
     x : captured piece, 6 bits (opposite side from p)
     E : 1 if en-passant move, 0 otherwise
     + : 1 if checking move, 0 otherwise

  Moves are created with the `game-compute-moves` function.  While you *can*
  create an `(unsigned-byte 32)` manually, or maybe via the `make-move`
  function, there is no guarantee that that move is valid in the context of an
  existing game.

- `game` -- a structure containing a board and other features that determine the
  current situation of a game: the side to move, the en-passant target field,
  the castling conditions, and the number of full-moves / half-moves.  Currently
  a `game` struct does not maintain a list of moves played so far (this might
  change).


Game functions
--------------

- (make-game) -- instantiate a `game` struct

- (game-board game) -- the board array

- (game-state game) -- the castling state

- (game-side game) -- the current side to play (+WHITE+ or +BLACK+)

- (game-enpa game) -- the index of the en-passant target field (or NIL)

- (game-fullmove game) -- the current move number

- (game-halfmove game) -- the "halfmove" number (number of moves since the
  last pawn or capturing move)

- (reset-from-fen game fen) -- reset a game from a FEN string or input
  stream

- (reset-game game) -- reset a game to starting position

- (game-fen game) -- return the current game position as a FEN string

- (game-move game move &optional quick) -- execute the given move.  When
  `quick` is true, skip updating the fullmove/halfmove counts.

- (with-move (game move &optional quick) &body body) -- macro; execute the
  move and perform body, then take back the move.

- (king-index game &optional side) -- return the `board-index` of the king
  for the given side (default to current side to play).

- (attacked? game &optional side index) -- return T if the given index/side
  is under attack.  Default arguments are the current side to play, and the
  king index.

- (game-compute-moves game) -- return the list of legal moves for the
  current side to play.

- (game-parse-san game san &optional moves) -- parse a move in SAN notation.
  If `moves` is not passed, it defaults to (game-compute-moves game).  This
  function returns a list of `move` elements; if SAN is valid, this list
  should contain exactly one move, but when it's not valid or ambiguous it
  might have zero or more elements.  `SAN` should be a string or an input
  stream.

- (game-san game move &optional moves) -- return a `move` in SAN notation.
  `moves` defaults to (game-compute-moves game).

- (draw-by-material? game) -- return T if the current position is a draw
  because of insufficient material.

- (perft game depth &optional count-mates) -- return PERFT results for the
  current game, to the given depth.  Pass true for `count-mates` if you'd
  like to count checkmates as well (slightly increases the run time).

- (divide game depth) -- used for debugging; see [1] for more information
  about PERFT and DIVIDE.


PGN parser
----------

(parse-pgn data) -- where `data` is a string or an input stream.  Returns a
plist like this:

    (:headers HEADERS
     :moves MOVES
     :game GAME)

HEADERS is an alist associating header name (as string) with the value (also
a string).

MOVES is a list of moves/comments/result found in the PGN data, in the order
they are encountered.  It has this form:

    ((:move . NUMBER) ... (:comment . "text") ... (:result . "1-0"))

The moves are in numerical representation described above.

GAME is a `game` instance in the final position.

Example:

    QUEEN> (parse-pgn "[White \"Foo\"]
    [Black \"Bar\"]

    1.d4 d5 2.c4 {Queen's gambit} dxc4 {accepted}")

    ==>

    (:HEADERS (("White" . "Foo")
               ("Black" . "Bar"))
     :MOVES ((:MOVE . 329419)
             (:MOVE . 67827)
             (:MOVE . 329354)
             (:COMMENT . "Queen's gambit")
             (:MOVE . 134284963)
             (:COMMENT . "accepted"))
     :GAME #S(GAME
              :BOARD #(...)
              :STATE 15
              :SIDE 64
              :ENPA NIL
              :FULLMOVE 3
              :HALFMOVE 0))


Piece functions
---------------

A piece is an 8 bit unsigned integer (only 7 bits are used).

- constants: +PAWN+, +KNIGHT+, +BISHOP+, +ROOK+, +QUEEN+, +KING+
             +WPAWN+, +WKNIGHT+, +WBISHOP+, +WROOK+, +WQUEEN+, +WKING+
             +WHITE+, +BLACK+

- (is-pawn? p), (is-knight? p), (is-bishop? p), (is-rook? p), (is-queen?
  p), (is-king? p) -- predicates that receive one argument (unsigned-byte 8)
  and return T if it's the respective piece.

- (is-white? p), (is-black? p) -- receive an (unsigned-byte 8) and return T
  if it's the respective color.

- (piece-side p) -- return the side of the piece (+WHITE+ or +BLACK+)

- (same-side? p1 p2) -- return T if the pieces are same color

- (piece-char p) -- return the piece as a character in standard chess
  notation, e.g.:

    R, N, B, Q, K, P for white pieces
    r, n, b, q, k, p for black pieces

  When *unicode* is T, use Unicode chess chars instead.

- (char-piece p) -- the inverse of `piece-char`


Board functions
---------------

A board is stored in a simple array of 120 `piece`-s.

- (board-index row col) -- return the index of the field with the given
  coords

- (field-index field) -- return the index of the named field.

      QUEEN> (field-index "d2")
      19
      QUEEN> (board-index 1 3)
      19
      QUEEN> $d2   ;; when using queen::syntax named readtable
      19

- (index-row index), (index-col index) -- return the coords (0..7)

- (index-field index) -- return the field name, e.g. "d2"

- (index-valid? index) -- return T if the index is valid, that is, it's an
  integer between 0 and 119 and it has the fourth and eighth bit clear.
  More exactly, a valid index looks like this:

    ┌─┬─┬─┬─┬─┬─┬─┬─┐
    │0│R│R│R│0│C│C│C│
    └─┴─┴─┴─┴─┴─┴─┴─┘

  Bits 0, 1, 2 are for the column, and 4, 5, 6 for the row.

- (board-get board index) -- return the piece at the given index (zero when
  the field is empty)

- (board-set board index piece) -- set the piece at the given index

- (board-get-rc board row col) -- return the piece at the given coords

- (board-set-rc board row col piece) -- set the piece at the given coords

- (board-foreach board fn) -- calls the given function for every piece of
  the board (skips empty fields).  The function is called with four
  arguments: piece, row, col and index.

- (print-board board :key (output t)) -- print the board to the given stream
  (by default it goes to standard output).


Move functions
--------------

You should not need to create `move`-s manually; obtain a list of moves via
`game-compute-moves`.  You can use the following accessors on a move (which
is just a 32 bit unsigned integer):

- (move-from move), (move-to move) -- return as `board-index` the move
  from/to fields

- (move-piece move) -- returns the moved piece

- (move-white? move), (move-black? move) -- return T if the moved piece is
  of the respective color

- (move-side move) -- return the move side (+WHITE+ or +BLACK+)

- (move-capture? move) -- return T if it's a capturing move

- (move-captured-piece move) -- returns the captured piece, or NIL if it's a
  non-capturing move

- (move-promote? move) -- return T if it's a promoting move

- (move-promoted-piece move) -- return the promoted piece, or NIL if it's
  not the case

- (move-check? move) -- return T if it's a checking move

- (move-enpa? move) -- return T if it's an en-passant move

- (move-captured-index move) -- return the index of the captured piece if
  it's a capturing move, NIL otherwise.  This can be different from (move-to
  move) in the case of en-passant moves.

- (move-oo? move) -- return T if it's a king-side castling move

- (move-ooo? move) -- return T if it's a queen-side castling move

- (move-castle? move) -- return T if it's a castling move


Evaluation
----------

I wrote this just for fun, and it's outside the scope of this library.  I
don't reasonably expect to produce a strong chess engine, compared to what's
available these days, but if nothing else, this helped me learn a lot about
optimizing Common Lisp code.  Before working on evaluation, (perft 6) ran in
12 minutes.  After optimization it's down to under 40 seconds [*].  Luckily,
this didn't require large refactoring of the code -- just inline trivial
functions, add type declarations, `(declare (optimize speed))` to certain
key functions, and listen to advice from SBCL (which is impressively smart).

  [*] For comparison, Crafty runs perft 6 in 4 seconds on my machine.  Now
      Crafty is a venerable, strong chess engine, written in hand-optimized
      C and using bitboards for move generation (the fastest known method; I
      couldn't wrap my brains around it).  I'd say 10x slower is Okay.

      I'm interested in the performance of other array-based move generators
      (i.e. not bitboards), if you know any please tell me.

Main entry point:

    (game-search game &optional (depth +MAX-DEPTH+))

It returns two values:

- a list of moves -- the "best line" that the algorithm could see.  Executed
  in sequence, these moves lead to the position that produces the best
  static evaluation score (see below) for the side to play, at the given
  depth (default: 5).

- the score for the final position.

What's implemented:

- NegaScout search algorithm (see [2]).  It's not exactly "principal
  variation search" yet, because we don't keep a history of researched
  positions and their score (which means our evaluation is prone to
  repetitions)

    (pvs game depth α β pline)

  It returns a score for the current position, and the best variant in pline
  (pass a (cons nil nil) and get the list of moves in the `car`).

- Quiescence search -- when NegaScout reaches depth zero, we try all
  capturing moves

    (quies game α β moves pline)

  This is called by pvs when it reaches depth=0.

The static evaluation function is based on [3].  It's symmetrical, in that
it cares for pieces of both sides — for example (static-value game) for the
start position returns zero.  A score bigger than zero means that the
current side to play looks better than the opponent.

The value is calculated by adding up the score for all our pieces, and
subtracting the score for all opponent pieces.  A piece has a material value
(the +MATx+ constants) and a certain score depending on where it's located
on the board (see scores in eval.lisp).

                                   * * *

[1] https://chessprogramming.wikispaces.com/Perft
[2] https://en.wikipedia.org/wiki/Principal_variation_search
[3] https://chessprogramming.wikispaces.com/Simplified+evaluation+function

                                   * * *

(c) Mihai Bazon 2016
License: MIT

Got questions? Let's talk.  <mihai.bazon@gmail.com>  http://lisperator.net
