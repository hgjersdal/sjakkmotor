(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :sjakk3)

(defmacro expand-game-tree ((game moves mv a2 fline
				  alpha beta castlings castle-row &optional (best-val (gensym))) &body eval-form)
  "Expand game-tree by calling move-eval-set with eval-form for every move in moves.
Does alpha-beta pruning, meaning it will return the highest value between alpha and beta,
 or alpha or beta. If the value of a move is equal to current alpha, it is set to 
-*max-eval* to make sure the best move goes to top in sort."
  `(progn
     (loop for ,mv of-type chess-move across ,moves
	with ,a2 of-type fixnum = ,alpha
	with ,best-val of-type fixnum  = ,a2
	  ,@fline
	do
	  (move-eval-set (,game ,mv ,castlings ,castle-row) ,@eval-form)
	  (let ((val (chess-move-evaluation ,mv)))
	    (when (= val ,a2) (setf (chess-move-evaluation ,mv) (- *max-eval*)))
	    (setf ,best-val (max val ,best-val))
	    (setf ,a2 (max ,a2 val)))
	  (when (>= ,a2 ,beta) (return ,best-val))
	finally (return ,best-val))))

(defun quiescence (game depth move alpha beta scale castlings opponent-castlings
		   index castle-row)
  "Quiescense search, called at horizon. Negamax with alpha-beta pruning. 
Expanding tree only for captures."
  (declare (type fixnum depth scale alpha beta index castle-row )
	   (type chess-move move)
	   (type castlings castlings opponent-castlings)
	   (type chess-game game))
  (with-slots (board move-arrays) game
    (let ((moves (aref move-arrays index))
	  (val (the fixnum (* scale (the fixnum (evaluate game))))))
      (declare (type fixnum val)
	       (type (simple-array fixnum (8 8)) board)
	       (type (vector chess-move 256) moves)
	       (type (simple-vector 21) move-arrays))
      (when (or (= 0 depth) (>= val beta))
	(return-from quiescence (the fixnum val)))
      (when (> val alpha)
	(setf alpha val))
      (push-moves game moves scale (chess-move-double-jump move)
		  (chess-move-old-col move) castlings)
      (when (opponent-move-illegalp board move moves (the fixnum (- 7 castle-row)))
	(setf (chess-move-legalp move) nil)
	(return-from quiescence (the fixnum *max-eval*)))
      (the fixnum 
	   (expand-game-tree (game moves m2 a2 (when (/= (chess-move-captured-val m2) 0))
				   alpha beta castlings castle-row)
	     (- (quiescence game (- depth 1) m2 (- beta) (- a2) (- scale)
			    opponent-castlings castlings (+ index 1) (- 7 castle-row))))))))
 
(defun negamaxit (game depth move alpha beta scale castlings opponent-castlings
		  qdepth index castle-row)
  "Negamax with alpha-beta pruning. Expanding tree for all moves."
  (declare (type fixnum depth scale alpha beta qdepth index castle-row *max-eval*)
	   (type chess-move move)
	   (type castlings castlings opponent-castlings)
	   (type chess-game game))
  (with-slots (board move-arrays) game
    (let ((moves (aref move-arrays index)))
      (setf (fill-pointer moves) 0)
      (when (= depth 0)
	(return-from negamaxit
	  (the fixnum (quiescence game qdepth move alpha beta scale castlings
				  opponent-castlings index castle-row))))
      (push-moves game moves scale (chess-move-double-jump move)
		  (chess-move-old-col move) castlings)
      (when (opponent-move-illegalp board move moves (- 7 castle-row))
	(setf (chess-move-legalp move) nil)
	(return-from negamaxit (the fixnum (+ *max-eval* depth))))
      (let ((best-val (- *max-eval*))
	    (legal-moves nil))
	(loop for d from 1 to depth 
	   with qd = 0 do
	     (setf legal-moves nil)
	     (when (= d depth)
	       (setf qd qdepth))
	     (setf best-val
		   (expand-game-tree (game moves m2 a2 ()
					   alpha beta castlings castle-row)
		     (prog1 (- (negamaxit game (- d 1) m2 (- beta) (- a2) (- scale)
					  opponent-castlings castlings
					  qd (+ index 1) (- 7 castle-row)))
		       (when (chess-move-legalp m2) (setf legal-moves t)))))
	     (when (not legal-moves)
	       (let ((m2 (make-chess-move)))
		 (negamaxit game 1 m2 (- *max-eval*) *max-eval* (- scale)
			    opponent-castlings castlings 0 (+ index 1) (- 7 castle-row))
		 (when (chess-move-legalp m2)
		   (setf (aref (move-arrays game) index) moves)
		   (return-from negamaxit (the fixnum 0)))))
	     (setf moves (sort moves (lambda (a b)
				       (> (chess-move-evaluation a)
					  (chess-move-evaluation b))))))
	(setf (aref (move-arrays game) index) moves)
	(when (= index 0) (setf (aref move-arrays 0) moves))
	(the fixnum best-val)))))

(defun timed-search (game move scale castlings opponent-castlings index castle-row search-time)
  "Negamax with alpha-beta pruning. Spend approximately time ms on the search. +/- 10% or so."
  (declare (type fixnum scale index castle-row *max-eval* search-time)
	   (type chess-move move)
	   (type castlings castlings opponent-castlings)
	   (type chess-game game))
  (with-slots (board move-arrays) game
    (let ((moves (aref move-arrays index))
	  (start-time (get-internal-real-time)))
      (setf (fill-pointer moves) 0)
      (push-moves game moves scale (chess-move-double-jump move)
		  (chess-move-old-col move) castlings)
      (let ((best-val (- *max-eval*))
	    (legal-moves nil))
	(loop for d from 1 to 20
	   when (> (- (get-internal-real-time) start-time) (* 0.7 search-time)) do
	     (return)
	   do
	     (setf legal-moves nil)
	     (let ((n-moves 0)
		   (do-stop nil))
	       (setf best-val
		     (expand-game-tree (game moves m2 a2
					     (when do-stop do (return bv))
					     (- *max-eval*) *max-eval* castlings
					     castle-row bv)
		       (prog1
			   (- (negamaxit game (- d 1) m2 (- *max-eval*) (- a2) (- scale)
					 opponent-castlings castlings
					 5 (+ index 1) (- 7 castle-row)))
			 (incf n-moves)
			 (when (> (- (get-internal-real-time) start-time) (* 1.0 search-time))
			   (setf (fill-pointer moves) n-moves)
			   (setf do-stop t)
			   (setf legal-moves t))
			 (when (chess-move-legalp m2) (setf legal-moves t))))))
	     (when (not legal-moves)
	       (let ((m2 (make-chess-move)))
		 (negamaxit game 1 m2 (- *max-eval*) *max-eval* (- scale)
			    opponent-castlings castlings 0 (+ index 1) (- 7 castle-row))
		 (when (chess-move-legalp m2)
		   (setf (aref (move-arrays game) index) moves)
		   (return-from timed-search (the fixnum 0)))))
	     (setf moves (sort moves (lambda (a b)
				       (> (chess-move-evaluation a)
					  (chess-move-evaluation b))))))
	;;(setf (aref move-arrays 0) moves)
	(the fixnum best-val)))))

(defun time-search (game whitep index &optional time)
  (let ((move (if (= (n-moves game) 0) (make-chess-move) (aref (played-moves game) (- (n-moves game) 1))))
	(scale (if whitep 1 -1))
	(castlings (if whitep (white-castle game) (black-castle game)))
	(opponent-castlings (if whitep (black-castle game) (white-castle game)))
	(castle-row (if whitep 0 7))
	(time (if time time (search-time game))))
    (timed-search game move scale castlings opponent-castlings index castle-row time)))

(defun depth-search (game depth qdepth whitep index)
  (let ((move (if (= (n-moves game) 0) (make-chess-move) (aref (played-moves game) (- (n-moves game) 1))))
	(scale (if whitep 1 -1))
	(castlings (if whitep (white-castle game) (black-castle game)))
	(opponent-castlings (if whitep (black-castle game) (white-castle game)))
	(castle-row (if whitep 0 7)))
    (negamaxit game depth move (- *max-eval*) *max-eval* scale castlings opponent-castlings qdepth index castle-row)))
