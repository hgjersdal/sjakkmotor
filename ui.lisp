;;Is the game finished? No legal moves? King in check?

;;(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :sjakk3)

(defmethod print-object ((chess-move chess-move) stream)
  "Rettyprint a chess move. Verbose, in that a4 is a2a4, for simplicity"
  (cond ((chess-move-castle-short chess-move) (format stream "O-O"))
	((chess-move-castle-long chess-move) (format stream "O-O-O"))
	(t (format stream "~a~a~a~a~a~a~a"
		   (if (= (abs (chess-move-old-val chess-move)) 1)
		       ""
		       (aref "  NBRQK" (abs (chess-move-old-val chess-move))))
		   (aref "abcdefgh" (chess-move-old-col chess-move))
		   (aref "12345678" (chess-move-old-row chess-move))
		   (if (= (chess-move-captured-val chess-move) 0) "" "x")
		   (aref "abcdefgh" (chess-move-new-col chess-move))
		   (aref "12345678" (chess-move-new-row chess-move))
		   (if (= (chess-move-new-val chess-move)
			  (chess-move-old-val chess-move))
		       ""
		       (format nil "=~a"
			       (aref "  NBRQK"
				     (abs (chess-move-new-val chess-move))))))))
  chess-move)

(defun piece-letter (piece)
  (aref " PNBRQK" (abs piece)))

(defun print-board (board)
  "ASCII chess board. White has CAPS piece-letters."
  (format t "~%")
  (loop for row from 7 downto 0 do
       (format t "~a ~a|" row (+ row 1))
       (loop for col from 0 to 7 do
	    (if (= (aref board col row) 0)
		(format t " |")
		(format t "~a|" (if (< (aref board col row) 0)
				    (char-downcase (piece-letter
						    (abs (aref board col row))))
				    (char-upcase (piece-letter
						  (abs (aref board col row))))))))
       (format t "~%"))
  (format t "    a b c d e f g h ~%    0 1 2 3 4 5 6 7 ~%"))

(defun human-move (game whitep fc fr tc tr)
  "Move a piece from fc,fr to tc,tr. Cols and rows go from 0-7."
  (with-slots (board n-moves white-castle black-castle played-moves) game
    (let ((moves (push-moves game (aref (move-arrays game) 0) (if whitep 1 -1)
			     (if (> n-moves 0) (chess-move-double-jump (aref played-moves n-moves)) nil)
			     (if (> n-moves 0) (chess-move-old-col (aref played-moves n-moves)) 0)
			     (if whitep white-castle black-castle))))
      (loop for move across moves 
	 when (and (= (chess-move-old-col move) fc)
		   (= (chess-move-old-row move) fr)
		   (= (chess-move-new-col move) tc)
		   (= (chess-move-new-row move) tr)
		   (or (= (chess-move-old-val move) (chess-move-new-val move))
		       (= (abs (chess-move-new-val move)) 5)))
	 do
	   (if whitep
	       (move-piece game move (white-castle game) 0)
	       (move-piece game move (black-castle game) 7))
	   (return)))))

(defun get-last-move (game)
  (with-slots (n-moves played-moves) game
    (if (= n-moves 0)
	(make-chess-move)
	(aref played-moves (- n-moves 1)))))

(defun analyze (game whitep &optional time)
  "Analyze position. Cannot be done by alpha-beta, each move from current position
must be searched independently."
  (with-slots (move-arrays n-moves) game
    (let ((move (get-last-move game))
	  (moves (aref move-arrays 0)))
      (push-moves game moves (if whitep 1 -1) (chess-move-double-jump move) (chess-move-old-col move)
		  (if whitep (white-castle game) (black-castle game)))
      (loop for move across moves do
	   (move-eval-set (game move (if whitep (white-castle game) (black-castle game)) (if whitep 0 7))
	     (sjakk3::time-search game whitep 1 time))
	   (format t "~a: ~a~%" move (chess-move-evaluation move)))
      (setf moves (sort moves (lambda (a b)
				(> (chess-move-evaluation a)
				   (chess-move-evaluation b)))))
      (loop for move across moves do
	   (format t "~a: ~a~%" move (chess-move-evaluation move))))))

(defun random-move (game whitep sorted-moves max-diff)
  "Pick a random move with a score that differs from the best score by no more than max-diff."
  (loop for move across sorted-moves do
       (format t "~a: ~a~%" move (chess-move-evaluation move)))
  (let ((eq-index 0))
    (loop for move across sorted-moves
       with top = (chess-move-evaluation (aref sorted-moves 0))
       when (> (chess-move-evaluation move) (- top max-diff)) do
	 (incf eq-index))
    (move-piece game (aref sorted-moves (random eq-index)) (if whitep (white-castle game) (black-castle game)) (if whitep 0 7))))

(defun computer-move (game whitep secret)
  "Computer move. Picks the move suggested by computer after a timed-search.
If not secret it prints value of position to t."
  (format t "Thinking...~%")
  (let ((val (time-search game whitep 0)))
    (let ((moves (aref (move-arrays game) 0)))
      (unless secret 
	(format t "Value is: ~a~%" val)
	(format t "Selecting ~a: ~a ~a~% " (floor (/ (n-moves game) 2))
		(if whitep "" "...")
		(aref moves 0)))
      (if whitep
	  (move-piece game (aref moves 0) (white-castle game) 0)
	  (move-piece game (aref moves 0) (black-castle game) 7)))))
