;;;; package.lisp

(defpackage #:queen
  (:use #:cl #:alexandria #:named-readtables #:anaphora)
  (:export #:+QUEEN+
           #:+ROOK+
           #:+KNIGHT+
           #:+BISHOP+
           #:+PAWN+
           #:+KING+

           #:+WQUEEN+
           #:+WROOK+
           #:+WKNIGHT+
           #:+WBISHOP+
           #:+WPAWN+
           #:+WKING+

           #:+WHITE+
           #:+FEN-START+

           #:is-pawn?
           #:is-knight?
           #:is-bishop?
           #:is-rook?
           #:is-queen?
           #:is-king?
           #:is-white?
           #:is-black?
           #:same-side?
           #:opp-side?

           #:board-get
           #:board-set
           #:with-piece
           #:board-foreach
           #:print-board
           #:*unicode*

           #:board-index
           #:index-valid?
           #:field-index
           #:index-field
           #:index-row
           #:index-col
           #:with-row-col
           #:piece-char
           #:char-piece
           #:*unicode*

           #:move
           #:move-from
           #:move-to
           #:move-piece
           #:move-black?
           #:move-white?
           #:move-side
           #:move-capture?
           #:move-captured-piece
           #:move-promote?
           #:move-promoted-piece
           #:move-set-promoted-piece
           #:move-enpa?
           #:move-captured-index
           #:move-oo?
           #:move-ooo?

           #:game
           #:make-game
           #:reset-from-fen
           #:reset-game
           #:game-fen
           #:game-move
           #:game-undo-move
           #:with-move
           #:king-index
           #:attacked?
           #:game-compute-moves
           #:game-parse-san
           #:game-san
           #:game-board
           #:game-state
           #:game-side
           #:game-enpa
           #:game-fullmove
           #:game-halfmove
           #:draw-by-material?

           #:parse-pgn
           #:game-search
           #:dump-line
           #:play
           ))

(in-package #:queen)

(defreadtable queen::syntax
  (:merge :standard))
