(asdf:defsystem #:queen
  :description "Chess utilities: board representation (0x88), move generation, PGN/SAN parser/generator"
  :author "Mihai Bazon <mihai.bazon@gmail.com>"
  :license "MIT"
  :depends-on (#:named-readtables
               #:anaphora
               #:alexandria
               #:cl-ppcre-unicode)
  :serial t
  :components ((:file "package")
               (:file "stream")
               (:file "board")
               (:file "pgn")
               (:file "eval")
               (:file "tests")))

(defpackage #:queen-config (:export #:*base-directory*))
(defparameter queen-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))
