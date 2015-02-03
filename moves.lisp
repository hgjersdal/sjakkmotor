(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :sjakk3)

(defun set-move (move oc or nc nr piece-val captured-val new-val
		 enpassant double-jump c-long c-short)
  "Set a move in the array of moves."
  (declare (type fixnum *max-eval* oc or nc nr piece-val captured-val new-val)
	   (type boolean double-jump enpassant c-long c-short)
	   (type chess-move move))
  (setf (chess-move-evaluation move) (- *max-eval*)
	(chess-move-old-col move) oc 
	(chess-move-old-row move) or
	(chess-move-new-col move) nc
	(chess-move-new-row move) nr
	(chess-move-old-val move) piece-val
	(chess-move-new-val move) new-val
	(chess-move-captured-val move) captured-val
	(chess-move-enpassant move) enpassant
	(chess-move-double-jump move) double-jump
	(chess-move-castle-long move) c-long
	(chess-move-castle-short move) c-short
	(chess-move-legalp move) t))

(defun push-move (moves oc or nc nr piece-val captured-val new-val
		  &key enpassant double-jump c-long c-short)
  "Add move to to the array of moves"
  (declare (type fixnum oc or nc nr piece-val captured-val new-val)
	   (type boolean enpassant double-jump c-long c-short)
	   (type (vector chess-move 256) moves))
  (set-move (aref moves (fill-pointer moves)) oc or nc nr piece-val captured-val
	    new-val enpassant double-jump c-long c-short)
  (incf (fill-pointer moves)))

(defun push-pawn-move (moves scale oc or nc nr piece-val captured-val trans-row
		       double-jump enpassant)
  "Add a pawn move to the array of moves. Pawns are special due to double jumps, enpassants"
  (declare (type fixnum scale oc or nc nr piece-val captured-val trans-row)
	   (type boolean double-jump enpassant)
	   (type (vector chess-move 256) moves))
  (if (= nr trans-row)
      (loop for val of-type fixnum from 2 to 5 do
	   (push-move moves oc or nc nr piece-val captured-val (the fixnum (* scale val))
		      :enpassant enpassant))
      (push-move moves oc or nc nr piece-val captured-val piece-val
		 :enpassant enpassant :double-jump double-jump)))

(defun push-pawn-take (board oc or nc nr piece-val moves trans-row scale double-jump
		  enpassant)
  "Push pawn take moves if possible."
  (declare (type fixnum scale oc or nc nr piece-val trans-row)
	   (type boolean double-jump enpassant)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (when (or (< nc 0) (> nc 7)) (return-from pawn-take))
  (let ((position-val (aref board nc nr)))
    (when (< (the fixnum (* position-val scale)) 0)
      (push-pawn-move moves scale oc or nc nr piece-val position-val trans-row
		      double-jump enpassant))))

(defun push-pawn-moves (board col row moves home-row transform-row enpassant-row
			double-jump-p double-jump-col scale)
  "Push pawn forward move. Double jump if legal."
  (declare (type fixnum col row home-row transform-row enpassant-row
		 double-jump-col scale)
	   (type boolean double-jump-p)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (push-pawn-take board col row (the fixnum (+ col 1)) (the fixnum (+ row scale)) scale moves transform-row
	     scale nil nil)
  (push-pawn-take board col row (the fixnum (- col 1)) (the fixnum (+ row scale)) scale moves transform-row
	     scale nil nil)
  (when (= (aref board col (+ row scale)) 0)
    (push-pawn-move moves scale col row col (the fixnum (+ row scale)) scale 0 transform-row
		    nil nil)
    (when (and (= row home-row) (= (aref board col (+ row (* 2 scale))) 0))
      (push-pawn-move moves scale col row col (the fixnum (+ row (the fixnum (* 2 scale)))) scale 0
		      transform-row t nil)))
  (when (and double-jump-p (= row enpassant-row) (= 1 (abs (- col double-jump-col))))
    (push-pawn-move moves scale col row double-jump-col (the fixnum (+ row scale)) scale
		    (the fixnum (- scale)) transform-row nil t)))
  
(defun maybe-take (oc or nc nr piece-val position-val scale moves)
  "If the attacked sqauare holds a piece of opposite color, take it."
  (declare (type fixnum oc or nc nr piece-val position-val scale)
	   (type (vector chess-move 256) moves))
  (when (<= (the fixnum (* position-val scale)) 0)
    (push-move moves oc or nc nr piece-val position-val piece-val)))

(macrolet ((loopy (line1 line2)
	      "Describe pieves moving in steps of delta row, delta col, 
where delta is one or zero"
	      `(loop ,@line1
		  ,@line2
		  when (= (aref board cc rr) 0) do
		  (push-move moves col row cc rr piece-val 0 piece-val)
		  else do (maybe-take col row cc rr piece-val (aref board cc rr)
				      scale moves)
		  (return))))
  
  (defun push-rook-moves (board col row moves scale)
    (declare (type fixnum col row scale)
	     (type (vector chess-move 256) moves)
	     (type (simple-array fixnum (8 8)) board))
    (let ((piece-val (aref board col row)))
      (loopy (for cc from (+ col 1) to 7) (with rr = row))
      (loopy (for cc from (- col 1) downto 0) (with rr = row))
      (loopy (for rr from (+ row 1) to 7) (with cc = col))
      (loopy (for rr from (- row 1) downto 0) (with cc = col))))

  (defun push-bishop-moves (board col row moves scale)
  (declare (type fixnum col row scale)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
    (let ((piece-val (aref board col row)))
      (loopy (for cc from (+ col 1) to 7) (for rr from (+ row 1) to 7))
      (loopy (for cc from (+ col 1) to 7) (for rr from (- row 1) downto 0))
      (loopy (for cc from (- col 1) downto 0) (for rr from (+ row 1) to 7))
      (loopy (for cc from (- col 1) downto 0) (for rr from (- row 1) downto 0)))))

(defun push-queen-moves (board col row moves scale)
  "Push queen moves. Simply the legal bishop moves from square and the legal rook moves."
  (declare (type fixnum col row scale)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (push-rook-moves board col row moves scale)
  (push-bishop-moves board col row moves scale))

(defun maybe-take-with-bounds (board oc or nc nr scale piece-val moves)
  "Maybe take, if attacked suqre is on the board."
  (declare (type fixnum oc or nc nr scale piece-val)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (when (and (> nc -1) (< nc 8) (> nr -1) (< nr 8))
    (maybe-take oc or nc nr piece-val (aref board nc nr) scale moves)))

(defun push-knight-moves (board col row moves scale)
  "Knight moves."
  (declare (type fixnum col row scale)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (loop for cc of-type fixnum in '(1 -1  1 -1 2 -2  2 -2)
     for rr of-type fixnum in    '(2  2 -2 -2 1  1 -1 -1)
     with piece-val = (aref board col row) do
       (maybe-take-with-bounds board col row (the fixnum (+ col cc)) (the fixnum (+ row rr)) scale piece-val
			       moves)))
	    
(defun push-king-moves (board col row moves scale)
  "King moves."
  (declare (type fixnum col row scale)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (let ((piece-val (aref board col row)))
    (loop for cc of-type fixnum from (- col 1) to (+ col 1) do
	 (loop for rr of-type fixnum from (- row 1) to (+ row 1) do
	      (maybe-take-with-bounds board col row cc rr scale piece-val moves)))))

(defun push-short-castle (board moves scale castlings castle-row)
  "Short castle move, if squares are open. "
  (declare (type fixnum scale castle-row)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (when (and (castlings-short castlings)
	     (= (aref board 5 castle-row) 0)
	     (= (aref board 6 castle-row) 0))
    (push-move moves 4 castle-row 6 castle-row (the fixnum (* scale 6)) 0  (the fixnum (* scale 6))
	       :c-short t)))

(defun push-long-castle (board moves scale castlings castle-row)
  "Long castle move."
  (declare (type fixnum scale castle-row)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (when (and (castlings-long castlings)
	     (= (aref board 1 castle-row) 0)
	     (= (aref board 2 castle-row) 0)
	     (= (aref board 3 castle-row) 0))
    (push-move moves 4 castle-row 2 castle-row (the fixnum (* scale 6)) 0 (the fixnum (* scale 6)) :c-long t)))

(defun king-capturep (board moves)
  "Does one of the moves take the king?"
  (declare (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board)
	   (ignorable board))
  (loop for move of-type chess-move across moves
     when (= 6 (abs (chess-move-captured-val move)))
     do (return t)
     finally (return nil)))

(defun field-is-attackedp (moves col row)
  "Does one of the moves attack the square col,row?"
  (declare (type (vector chess-move 256) moves)
	   (type fixnum col row))
  (loop for move of-type chess-move across moves
     when (and (= (chess-move-new-col move) col) (= (chess-move-new-row move) row))
     do (return t)))

(defun castle-still-legalp (move castlings castle-row)
  "Is castling still legal after the move?"
  (declare (type chess-move move)
	   (type castlings castlings)
	   (type fixnum castle-row))
  (when (and (= (chess-move-old-col move) 4)
	     (= (chess-move-old-row move) castle-row))
    (setf (castlings-long castlings) nil)
    (setf (castlings-short castlings) nil))
  (when (and (= (chess-move-old-col move) 0)
	     (= (chess-move-old-row move) castle-row))
    (setf (castlings-long castlings) nil))
  (when (and (= (chess-move-old-col move) 7)
	     (= (chess-move-old-row move) castle-row))
    (setf (castlings-short castlings) nil)))

(defun move-piece (game move castlings castle-row)
  "Move a piece. Check if it affects castling legality."
  (declare (type chess-move move)
	   (type chess-game game)
	   (type castlings castlings)
	   (type fixnum castle-row))
  (with-slots (n-moves played-moves board) game
    (declare (type (simple-array fixnum (8 8)) board)
	     (type fixnum n-moves)
	     (type (simple-vector 256) played-moves))
    (castle-still-legalp move castlings castle-row)
    (set-move (aref played-moves n-moves)
	      (chess-move-old-col move) (chess-move-old-row move)
	      (chess-move-new-col move) (chess-move-new-row move)
	      (chess-move-old-val move) (chess-move-captured-val move)
	      (chess-move-new-val move) (chess-move-enpassant move)
	      (chess-move-double-jump move) (chess-move-castle-long move)
	      (chess-move-castle-short move))
    (incf n-moves)
    (when (chess-move-castle-short move)
      (setf (aref board 5 castle-row) (aref board 7 castle-row))
      (setf (aref board 7 castle-row) 0))
    (when (chess-move-castle-long move)
      (setf (aref board 3 castle-row) (aref board 0 castle-row))
      (setf (aref board 0 castle-row) 0))
    (setf (aref board (chess-move-new-col move) (chess-move-new-row move))
	  (chess-move-new-val move))
    (setf (aref board (chess-move-old-col move) (chess-move-old-row move)) 0)
    (when (chess-move-enpassant move) 
      (setf (aref board (chess-move-new-col move) (chess-move-old-row move)) 0))))

(defun unmove-piece (game move castle-row)
  "Unmove a piece."
  (declare (type chess-move move)
	   (type chess-game game)
	   (type fixnum castle-row))
  (with-slots (board n-moves) game
    (declare (type (simple-array fixnum (8 8)) board)
	     (type fixnum n-moves))
    (decf n-moves)
    (when (chess-move-castle-short move)
      (setf (aref board 7 castle-row) (aref board 5 castle-row))
      (setf (aref board 5 castle-row) 0))
    (when (chess-move-castle-long move)
      (setf (aref board 0 castle-row) (aref board 3 castle-row))
      (setf (aref board 3 castle-row) 0))
    (setf (aref board (chess-move-new-col move) (chess-move-new-row move))
	  (chess-move-captured-val move))
    (setf (aref board (chess-move-old-col move) (chess-move-old-row move))
	  (chess-move-old-val move))
    (when (chess-move-enpassant move)
      (setf (aref board (chess-move-new-col move) (chess-move-new-row move)) 0)
      (setf (aref board (chess-move-new-col move) (chess-move-old-row move))
	    (chess-move-captured-val move)))))

(defmacro move-eval-set ((game move castlings castle-row) &body eval-form)
  "Move a piece, set move-evaluation to eval-form, unmove-piece, reset castling rights."
  (let ((clong (gensym))
	(cshort (gensym)))
    `(let ((,clong (castlings-long ,castlings))
	   (,cshort (castlings-short ,castlings)))
       (move-piece ,game ,move ,castlings ,castle-row)
       (setf (chess-move-evaluation ,move) ,@eval-form)
       (unmove-piece ,game ,move ,castle-row)
       (setf (castlings-long ,castlings) ,clong)
       (setf (castlings-short ,castlings) ,cshort))))

(defun push-moves (game moves scale double-jump-p double-jump-col can-castle)
  "Push all the legal moves on the board"
  (declare (type fixnum scale double-jump-col)
	   (type (vector chess-move 256) moves)
	   (type chess-game game)
	   (type castlings can-castle)
	   (type boolean double-jump-p))
  (let ((board (board game)))
    (declare (type (simple-array fixnum (8 8)) board))
    (setf (fill-pointer moves) 0)
    (loop for row of-type fixnum from 0 to 7 do
	 (loop for col of-type fixnum  from 0 to 7
	    when (> (the fixnum (* scale (aref board col row))) 0) do
	      (ecase (abs (aref board col row))
		(1 (if (= scale 1)
		       (push-pawn-moves board col row moves 1 7 4 double-jump-p
					double-jump-col scale)
		       (push-pawn-moves board col row moves 6 0 3 double-jump-p
					double-jump-col scale)))
		(2 (push-knight-moves board col row moves scale))
		(3 (push-bishop-moves board col row moves scale))
		(4 (push-rook-moves board col row moves scale))
		(5 (push-queen-moves board col row moves scale))
		(6 (push-king-moves board col row moves scale)))))
    (let ((castle-row (if (> scale 0) 0 7)))
      (push-short-castle board moves scale can-castle castle-row)
      (when (castlings-long can-castle)
	(push-long-castle board moves scale can-castle castle-row)))
    moves))

(defun opponent-move-illegalp (board move moves opponent-castle-row)
  "Was move 'move' legal? Can the king be taken? Are squares that affect
castling rights attacked? Check must be performed after call to 
(move-piece move)" 
  (declare (type chess-move move)
	   (type (vector chess-move 256) moves)
	   (type (simple-array fixnum (8 8)) board))
  (or (king-capturep board moves)
      (and (chess-move-castle-long move)
	   (or (field-is-attackedp moves 4 opponent-castle-row)
	       (field-is-attackedp moves 3 opponent-castle-row)))
      (and (chess-move-castle-short move)
	   (or (field-is-attackedp moves 4 opponent-castle-row)
	       (field-is-attackedp moves 5 opponent-castle-row)))))
