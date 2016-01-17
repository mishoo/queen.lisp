;;;; cl-chess.asd

(asdf:defsystem #:cl-chess
  :description "Describe cl-chess here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:named-readtables #:anaphora)
  :serial t
  :components ((:file "package")
               (:file "cl-chess")
               (:file "board")))

