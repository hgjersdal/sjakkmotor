(declaim (optimize (speed 3) (debug 0) (safety 0)))

(in-package :sjakk)

;;;If any move in list of moves can take king, stop search, return +/- max
;;;If castle, check that new rook field is not attacked. If so, stop search, return +/- max
;;;If king not capturable, and no legal moves, 0

(defun move-illegalp (board move castle-row moves)
  (or (king-capturep board moves)
      (and (chess-move-castle-long move)
	   (field-is-attackedp moves 3 castle-row))
      (and (chess-move-castle-short move) ;;Was the castle move legal?
	   (field-is-attackedp moves 5 castle-row))))

(defun negamax (board move depth alpha beta whitep castlings)
  (let ((can-castle (if whitep (can-castle-white castlings) (can-castle-black castlings)))
	(scale (if whitep 1 -1))
	(castle-row (if whitep 0 7)))
    (when (= depth 0)
      ;;(setf (chess-move-evaluation move) (* scale (evaluate board)))
      (return-from negamax (* scale (evaluate board))))
    (when (or (car can-castle) (cdr can-castle))
      (castle-still-legalp move can-castle castle-row))
    (let* ((moves (list-moves board can-castle whitep (chess-move-double-jump move)
			      (chess-move-old-col move)))
	   (best-move nil))
      (when (move-illegalp board move castle-row moves)
       	(return-from negamax (values 32500 nil)))
      (loop for m2 in moves
	 with val = 0
	 with best-val = alpha do
	   (move-piece board m2 castle-row)
	   (setf val (- (negamax board m2 (- depth 1) (- beta) (- alpha) (not whitep)
				 (copy-castle castlings))))
	   (unmove-piece board m2 castle-row)
	   (when (> val best-val)
	     (setf best-val val)
	     (setf best-move m2))
	   (setf alpha (max alpha val))
	   (when (>= alpha beta)
	     (return (values best-val best-move)))
	 finally
	   (return (values best-val best-move))))))

(defun alpha-beta (board move depth alpha beta whitep castlings)
  (let ((can-castle (if whitep (can-castle-white castlings) (can-castle-black castlings)))
	(scale (if whitep 1 -1))
	(castle-row (if whitep 0 7)))
    (when (= depth 0)
      (setf (chess-move-evaluation move) (* scale (evaluate board)))
      (return-from alpha-beta))
    (when (or (car can-castle) (cdr can-castle))
      (castle-still-legalp move can-castle castle-row))
    (let ((moves (list-moves board can-castle whitep (chess-move-double-jump move)
			     (chess-move-old-col move))))
      ;; (when (move-legalp board move castle-row moves)
      ;; 	(setf (chess-move-evaluation move) 32500)
      ;; 	(return-from alpha-beta))
      (loop for depth from 1 to depth do
      	   (loop for m in moves
      	      with val = alpha do
      		(move-piece board m castle-row)
      		(alpha-beta board m (- depth 1) (- beta) (- val) (not whitep)
      			    (copy-castle castlings))
      		(unmove-piece board m castle-row)
      		(let ((abval (chess-move-evaluation m)))
      		  (when (> abval val) (setf val abval)))
      		(when (>= val beta)
      		  (setf (chess-move-evaluation m) val)
      		  (return))
      	      finally 
      		(setf (chess-move-evaluation m) val))
      	   (setf moves (sort moves (lambda (a b) (> (* scale (chess-move-evaluation a))
      						    (* scale (chess-move-evaluation b)))))))
      (setf (chess-move-evaluation move) (- (chess-move-evaluation (car moves))))
      (car moves))))
      
  
  
(defun negamaxit (board move depth alpha beta whitep castlings)
  (let ((can-castle (if whitep (can-castle-white castlings) (can-castle-black castlings)))
	(scale (if whitep 1 -1))
	(castle-row (if whitep 0 7)))
    (when (= depth 0)
      (return-from negamaxit (* scale (evaluate board))))
    (when (or (car can-castle) (cdr can-castle))
      (castle-still-legalp move can-castle castle-row))
    (let ((moves (list-moves board can-castle whitep (chess-move-double-jump move)
			     (chess-move-old-col move)))
	  (best-move (make-chess-move))
	  (best-val 0))
      (when (move-illegalp board move castle-row moves)
       	(return-from negamaxit (values 32500 nil)))
      (loop for d from 1 to depth
	 with al = 0 do
	   (setf al alpha)
	   (setf best-val al)
	   (loop for m2 in moves
	      with val = 0 do
		(move-piece board m2 castle-row)
		(setf val (- (negamaxit board m2 (- d 1) (- beta) (- al) (not whitep)
					(copy-castle castlings))))
		(setf (chess-move-evaluation m2) val)
		(unmove-piece board m2 castle-row)
		(when (and (= depth 7)
			   (= (aref board 2 4) 0))
		  (format t "Vanished: ~a~%" m2)
		  (return))
		(when (> val best-val)
		  (setf best-val val)
		  (setf best-move m2))
		(setf al (max al val))
		(when (>= al beta)
		  (return)))
	   (setf moves (sort moves (lambda (a b)
				     (> (chess-move-evaluation a)
					(chess-move-evaluation b))))))
      (values best-val best-move))))
			       

