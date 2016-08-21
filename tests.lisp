(in-package #:queen)

(defvar *perft-pathname*
  (merge-pathnames #p"perftsuite.epd" queen-config:*base-directory*))

(defun run-perft-tests (&optional (depth 3))
  (with-open-file (input *perft-pathname*
                         :direction :input
                         :element-type 'character)
    (with-parse-stream input
      (let ((failed 0)
            (total 0))
        (loop until (progn
                      (skip-whitespace)
                      (eof?))
              for game = (make-game)
              do (reset-from-fen game input)
                 (format t "~A~%" (game-fen game))
                 (print-board (game-board game))
                 (incf total)
                 (let ((results
                         (loop until (char= (peek) #\Newline)
                               collect
                               (cons (progn
                                       (skip-whitespace)
                                       (skip #\;)
                                       (skip #\D)
                                       (read-number))
                                     (progn
                                       (skip-whitespace)
                                       (read-number))))))
                   (let ((count (perft game depth))
                         (result (cdr (assoc depth results))))
                     (cond
                       ((= count result)
                        (format t "PASS ~A~%" count))
                       (t
                        (format t "FAIL ~A, should be ~A~%~A~%"
                                count result results)
                        (incf failed))))
                   (format t "~%~%")))
        (format t "TOTAL: ~A, FAILED: ~A~%" total failed)))))
