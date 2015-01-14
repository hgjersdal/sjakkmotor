(declaim (optimize (speed 3) (debug 0) (safety 0)))

(in-package :sjakk)

(defstruct chess-move 
  (old-col 0 :type (unsigned-byte 8))
  (old-row 0 :type (unsigned-byte 8))
  (new-col 0 :type (unsigned-byte 8))
  (new-row 0 :type (unsigned-byte 8))
  (old-val 0 :type (signed-byte 8))
  (new-val 0 :type (signed-byte 8)) ;;Needed for pawn transform
  (captured-val 0 :type (signed-byte 8))
  (evaluation 0 :type (signed-byte 16))
  (enpassant nil :type boolean)
  (double-jump nil :type boolean)
  (castle-short nil :type boolean)
  (castle-long nil :type boolean))

(defmethod print-object ((chess-move chess-move) stream)
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
		   (if (= (chess-move-new-val chess-move) (chess-move-old-val chess-move))
		       ""
		       (format nil "=~a" (aref "  NBRQK" (abs (chess-move-new-val chess-move))))))))
  chess-move)

(defparameter *max-eval* 32500)

(defun make-move (old-col old-row new-col new-row old-val
		  &optional  (capture-val 0) new-val enpassant double-jump)
  (when (null new-val)(setf new-val old-val))
  (make-chess-move :old-col old-col :old-row old-row
		   :new-col new-col :new-row new-row
		   :old-val old-val :new-val new-val
		   :captured-val capture-val :enpassant enpassant
		   :double-jump double-jump
		   :evaluation (if (> old-val 0) (* -1 *max-eval*) (* 1 *max-eval*))))

(defun push-move (moves old-col old-row new-col new-row old-val
		  &optional (capture-val 0) new-val double-jump enpassant)
  (cons (make-move old-col old-row new-col new-row old-val capture-val new-val enpassant double-jump)
	moves))

(defmacro pawn-strike (board col delta-col row delta-row piece-capture-test
		       not-at-side second-to-last moves)
  `(progn (when ,not-at-side
	    (let ((capture-val (aref ,board (+ ,col ,delta-col) (+ ,row ,delta-row))))
	      (when (,piece-capture-test capture-val 0)
		(if ,second-to-last ;;Promote?
		    (loop for val from 2 to 5 do
			 (setf ,moves (push-move ,moves ,col ,row (+ ,col ,delta-col) (+ ,row ,delta-row)
						 ,delta-row capture-val (* ,delta-row val))))
		    (setf ,moves (push-move ,moves ,col  ,row (+ ,col ,delta-col) (+ ,row ,delta-row) 
					    ,delta-row capture-val))))))
	  ,moves))

(defmacro pawn-move (board col row delta-row second-to-last at-start moves)
  `(progn (when (= (aref ,board ,col (+ ,row ,delta-row)) 0)
	    (if ,second-to-last ;;Promote?
		(loop for val from 2 to 5 do
		     (setf ,moves (push-move ,moves ,col  ,row ,col (+ ,row ,delta-row)
					     ,delta-row 0 (* ,delta-row val))))
		(setf ,moves (push-move ,moves ,col ,row
					,col (+ ,row ,delta-row) ,delta-row)))
	    (when (and ,at-start (= (aref ,board ,col (+ ,row (* 2 ,delta-row))) 0)) ;;2 steps
	      (setf ,moves (push-move ,moves ,col ,row ,col (+ ,row (* 2 ,delta-row))
				      ,delta-row 0 ,delta-row t nil))))
	  ,moves))

(defmacro enpassant-move (col row delta-row double-jump-col enpassant-row moves)
  `(progn (when (and (= ,row ,enpassant-row) (= (abs (- ,double-jump-col ,col)) 1))
	    (setf ,moves (push-move ,moves ,col ,row ,double-jump-col (+ ,row ,delta-row)
				    ,delta-row (- ,delta-row) ,delta-row nil t)))
	  ,moves))

(defun list-white-pawn-moves (board col row moves double-jump-p double-jump-col)
  (setf moves (pawn-strike board col -1 row 1 < (> col 0) (= row 6) moves))
  (setf moves (pawn-strike board col  1 row 1 < (< col 7) (= row 6) moves))
  (setf moves (pawn-move board col row 1 (= row 6) (= row 1) moves))
  (when double-jump-p (setf moves (enpassant-move col row 1 double-jump-col 4 moves)))
  moves)

(defun list-black-pawn-moves (board col row moves double-jump-p double-jump-col)
  (setf moves (pawn-strike board col -1 row -1 > (> col 0) (= row 1) moves))
  (setf moves (pawn-strike board col  1 row -1 > (< col 7) (= row 1) moves)) 
  (setf moves (pawn-move board col row -1 (= row 1) (= row 6) moves))
  (when double-jump-p (setf moves (enpassant-move col row -1 double-jump-col 3 moves)))
  moves)

(defun maybe-take (old-col old-row new-col new-row piece-val position-val whitep moves)
  (when (and whitep (< position-val 0))
    (setf moves (push-move moves old-col old-row new-col new-row piece-val position-val)))
  (when (and (not whitep) (> position-val 0))
    (setf moves (push-move moves old-col old-row new-col new-row piece-val position-val)))
  moves)

(macrolet ((loopy (line1 line2)
	      `(loop ,@line1
		  ,@line2
		  when (= (aref board cc rr) 0) do
		  (setf moves (push-move moves col row cc rr piece-val))
		  else do (setf moves (maybe-take col row cc rr piece-val
						  (aref board cc rr) whitep moves))
		  (return))))
  
  (defun list-rook-moves (board col row moves whitep)
    (let ((piece-val (aref board col row)))
      (loopy (for cc from (+ col 1) to 7) (with rr = row))
      (loopy (for cc from (- col 1) downto 0) (with rr = row))
      (loopy (for rr from (+ row 1) to 7) (with cc = col))
      (loopy (for rr from (- row 1) downto 0) (with cc = col)))
    moves)

  (defun list-bishop-moves (board col row moves whitep)
    (let ((piece-val (aref board col row)))
      (loopy (for cc from (+ col 1) to 7) (for rr from (+ row 1) to 7))
      (loopy (for cc from (+ col 1) to 7) (for rr from (- row 1) downto 0))
      (loopy (for cc from (- col 1) downto 0) (for rr from (+ row 1) to 7))
      (loopy (for cc from (- col 1) downto 0) (for rr from (- row 1) downto 0)))
    moves))

(defun list-queen-moves (board col row moves whitep)
  (setf moves (list-rook-moves board col row moves whitep))
  (setf moves (list-bishop-moves board col row moves whitep))
  moves)

(defun maybe-take-with-bounds (board col row cc rr whitep piece-val moves)
  (when (and (> cc -1) (< cc 8) (> rr -1) (< rr 8))
    (if (= 0 (aref board cc rr))
	(setf moves (push-move moves col row cc rr piece-val))
	(setf moves (maybe-take col row cc rr piece-val (aref board cc rr) whitep moves))))
  moves)

(defun list-knight-moves (board col row moves whitep)
  (loop for cc in '(1 -1  1 -1 2 -2  2 -2)
     for rr in    '(2  2 -2 -2 1  1 -1 -1)
     with piece-val = (aref board col row) do
       (setf moves (maybe-take-with-bounds board col row (+ col cc) (+ row rr) whitep piece-val moves)))
  moves)
	    
(defun list-king-moves (board col row moves whitep)
  (let ((piece-val (aref board col row)))
    (loop for cc from (- col 1) to (+ col 1) do
       (loop for rr from (- row 1) to (+ row 1) do
	    (setf moves (maybe-take-with-bounds board col row cc rr whitep piece-val moves)))))
  moves)

(defun list-short-castle (board moves whitep)
  (let ((castle-row (if whitep 0 7)))
    (when (and (= (aref board 5 castle-row) 0)
	       (= (aref board 6 castle-row) 0))
      (setf moves (cons (make-chess-move :old-col 4 :old-row castle-row
					 :new-col 6 :new-row castle-row
					 :old-val (aref board 4 castle-row)
					 :new-val (aref board 4 castle-row)
					 :castle-short t :evaluation
					 (if (> (aref board 4 castle-row) 0) (* -1 *max-eval*) (* 1 *max-eval*)))
			moves))))
  moves)

(defun list-long-castle (board moves whitep)
  (let ((castle-row (if whitep 0 7)))
    (when (and (= (aref board 1 castle-row) 0)
	       (= (aref board 2 castle-row) 0)
	       (= (aref board 3 castle-row) 0))
      (setf moves (cons (make-chess-move :old-col 4 :old-row castle-row
					 :new-col 2 :new-row castle-row
					 :old-val (aref board 4 castle-row)
					 :new-val (aref board 4 castle-row)
					 :castle-long t :evaluation
					 (if (> (aref board 4 castle-row) 0) (* -1 *max-eval*) (* 1 *max-eval*)))
			moves))))
  moves)

(defun king-capturep (board moves)
  (loop for move in moves
     when (= (piece-type-to-val :king)
	     (piece-type (aref board (chess-move-new-col move)
			       (chess-move-new-row move)))) do
       (return t)))

(defun field-is-attackedp (moves col row)
  (loop for move in moves
     when (and (= (chess-move-new-col move) col)
	       (= (chess-move-new-row move) row)) do
       (return t)))

(defun castle-still-legalp (move can-castle castle-row)
  (when (and (= (chess-move-old-col move) 4) ;;King move
	     (= (chess-move-old-row move) castle-row))
    (setf (car can-castle) nil
	  (cdr can-castle) nil))
  (when (and (= (chess-move-old-col move) 0) ;;Left rook
	     (= (chess-move-old-row move) castle-row))
    (setf (cdr can-castle) nil))
  (when (and (= (chess-move-old-col move) 7) ;;Right rook
	     (= (chess-move-old-row move) castle-row))
    (setf (car can-castle) nil)))

(defun move-piece (board move castle-row)
  (when (chess-move-castle-short move)
    (setf (aref board 5 castle-row) (aref board 7 castle-row))
    (setf (aref board 7 castle-row) 0))
  (when (chess-move-castle-long move)
    (setf (aref board 3 castle-row) (aref board 0 castle-row))
    (setf (aref board 0 castle-row) 0))
  (setf (aref board (chess-move-new-col move) (chess-move-new-row move)) (chess-move-new-val move))
  (setf (aref board (chess-move-old-col move) (chess-move-old-row move)) 0)
  (when (chess-move-enpassant move) 
    (setf (aref board (chess-move-new-col move) (chess-move-old-row move)) 0))
  (when (chess-move-enpassant move) 
    ;;Original position 0
    ;;Ahead of original: 1
    ;;two ahead of original 0
    ;;next to two ahead of original 0
    ))

(defun unmove-piece (board move castle-row)
  (when (chess-move-castle-short move)
    (setf (aref board 7 castle-row) (aref board 5 castle-row))
    (setf (aref board 5 castle-row) 0))
  (when (chess-move-castle-long move)
    (setf (aref board 0 castle-row) (aref board 3 castle-row))
    (setf (aref board 3 castle-row) 0))
  (setf (aref board (chess-move-new-col move) (chess-move-new-row move)) (chess-move-captured-val move))
  (setf (aref board (chess-move-old-col move) (chess-move-old-row move)) (chess-move-old-val move))
  (when (chess-move-enpassant move)
    (setf (aref board (chess-move-new-col move) (chess-move-new-row move)) 0)
    (setf (aref board (chess-move-new-col move) (chess-move-old-row move)) (chess-move-captured-val move)))
  (when (chess-move-enpassant move)
    ;; original pawn position should be 0
    ;; position ahead of original should be 0
    ;; To steps ahead should be 1
    ;; Col next to two steps ahead should be 1
    ))

(defun list-moves (board can-castle whitep double-jump-p double-jump-col)
  (let ((moves nil)
	(whitep-factor (if whitep 1 -1)))
    (loop for row from 0 to 7 do
	 (loop for col from 0 to 7
	    when (> (* whitep-factor (aref board col row)) 0) do
	      (ecase (abs (aref board col row))
		(1 (if whitep
		       (setf moves (list-white-pawn-moves board col row moves double-jump-p double-jump-col))
		       (setf moves (list-black-pawn-moves board col row moves double-jump-p double-jump-col))))
		(2 (setf moves (list-knight-moves board col row moves whitep)))
		(3 (setf moves (list-bishop-moves board col row moves whitep)))
		(4 (setf moves (list-rook-moves board col row moves whitep)))
		(5 (setf moves (list-queen-moves board col row moves whitep)))
		(6 (setf moves (list-king-moves board col row moves whitep))))))
    (when (car can-castle)
      (setf moves (list-short-castle board moves whitep)))
    (when (cdr can-castle)
      (setf moves (list-long-castle board moves whitep)))
    moves))

;;; Check that the new rook space is not under attack when catling
;;; Copy the conses for catling every move
;;; Check that the king is not in check
;;; Check if we are in the endgame
