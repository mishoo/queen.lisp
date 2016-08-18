(asdf:defsystem #:queen
  :description "Chess utilities: board representation (0x88), move generation, PGN/SAN parser/generator"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license "MIT"
  :depends-on (#:named-readtables
               #:anaphora
               #:cl-ppcre-unicode)
  :serial t
  :components ((:file "package")
               (:file "stream")
               (:file "board")
               (:file "pgn")
               (:file "eval")))
