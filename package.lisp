;;;; package.lisp

(defpackage #:cl-chess
  (:use #:cl #:alexandria #:named-readtables #:anaphora)
  (:export #:syntax

           #:C.QUEEN
           #:C.ROOK
           #:C.KNIGHT
           #:C.BISHOP
           #:C.PAWN
           #:C.KING
           #:C.WHITE

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

           ;; debug functions
           #:perft
           #:divide
           ))

(in-package #:cl-chess)

(defreadtable cl-chess:syntax
  (:merge :standard))
