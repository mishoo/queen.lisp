;;;; cl-chess.asd

(asdf:defsystem #:cl-chess
  :description "Describe cl-chess here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria
               #:named-readtables
               #:anaphora
               #:cl-ppcre
               #:cl-ppcre-unicode)
  :serial t
  :components ((:file "package")
               (:file "cl-chess")
               (:file "stream")
               (:file "board")
               (:file "pgn")
               (:file "eval")))

