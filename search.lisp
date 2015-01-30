(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :sjakk2)

(defmacro move-eval-set ((game move castlings castle-row) &body eval-form)
  (let ((clong (gensym))
	(cshort (gensym)))
    `(let ((,clong (castlings-long ,castlings))
	   (,cshort (castlings-short ,castlings)))
       (move-piece ,game ,move ,castlings ,castle-row)
       (setf (chess-move-evaluation ,move) ,@eval-form)
       (unmove-piece ,game ,move ,castle-row)
       (setf (castlings-long ,castlings) ,clong)
       (setf (castlings-short ,castlings) ,cshort))))

(defmacro alpha-beta ((game moves mv a2 fline
			    alpha beta castlings castle-row) &body eval-form)
  (let ((best-val (gensym)))
    `(loop for ,mv across ,moves
	with ,a2 = ,alpha
	with ,best-val = ,a2
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
  (declare (type fixnum depth scale alpha beta index)
	   (type chess-move move)
	   (type castlings castlings opponent-castlings)
	   (type chess-game game))
  (with-slots (board move-arrays) game
    (let ((moves (aref move-arrays index))
	  (val (* scale (the fixnum (evaluate (board game))))))
      (when (or (= 0 depth) (>= val beta))
	(return-from quiescence (the fixnum val)))
      (when (> val alpha)
	(setf alpha val))
      (push-moves game moves scale (chess-move-double-jump move)
		  (chess-move-old-col move) castlings)
      (when (opponent-move-illegalp board move moves (- 7 castle-row))
	(setf (chess-move-legalp move) nil)
	(return-from quiescence (the fixnum *max-eval*)))
      (the fixnum 
	   (alpha-beta (game moves m2 a2 (when (/= (chess-move-captured-val m2) 0))
			     alpha beta castlings castle-row)
	     (- (quiescence game (- depth 1) m2 (- beta) (- a2) (- scale)
			    opponent-castlings castlings (+ index 1) (- 7 castle-row))))))))

(defun negamaxit (game depth move alpha beta scale castlings opponent-castlings
		  qdepth index castle-row)
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
	(return-from negamaxit (the fixnum *max-eval*)))
      (let ((best-val (- *max-eval*))
	    (legal-moves nil))
	(loop for d from 1 to depth 
	   with qd = 0 do
	     (when (= d depth)
	       (setf qd qdepth))
	     (setf best-val
		   (alpha-beta (game moves m2 a2 ()
				     alpha beta castlings castle-row)
		     (progn (when (chess-move-legalp m2) (setf legal-moves t))
			    (- (negamaxit game (- d 1) m2 (- beta) (- a2) (- scale)
					  opponent-castlings castlings
					  qd (+ index 1) (- 7 castle-row))))))
	     (when (and (= best-val (- *max-eval*)) (not legal-moves))
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
