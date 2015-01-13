(in-package :sjakk)
;;Enpassant stole the pawn! Check undo move

(defun test-moves ()
  (let ((board (make-array (list 8 8) :element-type '(signed-byte 8)
			   :initial-element 0))
	(moves nil))
    (setf (aref board 4 0) (make-piece t :king))
    (setf (aref board 7 0) (make-piece t :rook))
    (setf (aref board 0 0) (make-piece t :rook))
    (setf (aref board 2 0) (make-piece t :knight))
    (setf (aref board 0 6) (make-piece t :pawn))
    (setf (aref board 2 4) (make-piece t :pawn))

    (setf (aref board 7 5) (make-piece nil :queen))
    (setf (aref board 0 7) (make-piece nil :king))
    (setf (aref board 1 5) (make-piece nil :rook))
    (setf (aref board 3 4) (make-piece nil :pawn))

    (print-board board)
    (setf moves (list-moves board (cons t t) t t 3))
    (format t "White moves:~%~a~%" moves)
    (format t "Check?: ~a~%" (if (king-capturep board moves) "yes" "no"))
    (format t "Evaluate: ~a~%" (evaluate board))
    (let ((move (alpha-beta board (make-chess-move) 4 -32500 32500 t (make-can-castle))))
      (format t "~a,~a~%" move (chess-move-evaluation move)))))



(defun test-moves ()
  (let ((board (make-array (list 8 8) :element-type '(signed-byte 8)
  			   :initial-element 0))
  	(moves nil))
    (setf (aref board 0 1) (make-piece t :pawn))
    (setf (aref board 1 1) (make-piece t :pawn))
    (setf (aref board 2 1) (make-piece t :pawn))
    (setf (aref board 3 3) (make-piece t :pawn))
    (setf (aref board 4 4) (make-piece t :pawn))
    (setf (aref board 5 2) (make-piece t :pawn))
    (setf (aref board 7 1) (make-piece t :pawn))

    (setf (aref board 0 0) (make-piece t :rook))
    (setf (aref board 6 0) (make-piece t :rook))

    (setf (aref board 4 3) (make-piece t :knight))
    (setf (aref board 6 4) (make-piece t :bishop))

    (setf (aref board 3 0) (make-piece t :queen))
    (setf (aref board 7 2) (make-piece t :king))

    (setf (aref board 0 4) (make-piece nil :pawn))
    (setf (aref board 1 4) (make-piece nil :pawn))
    (setf (aref board 5 4) (make-piece nil :pawn))
    (setf (aref board 7 6) (make-piece nil :pawn))

    (setf (aref board 0 7) (make-piece nil :rook))
    (setf (aref board 7 7) (make-piece nil :rook))

    (setf (aref board 2 7) (make-piece nil :bishop))
    (setf (aref board 3 6) (make-piece nil :knight))
    (setf (aref board 5 6) (make-piece nil :queen))
    (setf (aref board 4 5) (make-piece nil :king))

    
    (let ((move (alpha-beta board (make-chess-move) 1 -32500 32500 t (make-can-castle))))
      (format t "~a,~a~%" move (chess-move-evaluation move)))))
    
(test-moves)

(defparameter *board* (make-initial-board))
(defparameter *can-castle* (make-can-castle))
(defparameter *last-move* (make-chess-move))

(defun new-game ()
  (setf *board* (make-initial-board)
	*can-castle* (make-can-castle)
	*last-move* (make-chess-move)))
  
(defun computer-move ()
  (format t "Processing!~%")
  ;; (multiple-value-bind (val move)
  ;;     (time (negamax *board* *last-move* 7 -32500 32500 t *can-castle*))
  ;;   (format t "~a: ~a~%" move val))
  (multiple-value-bind (val move)
      (time (negamaxit *board* *last-move* 7 -32500 32500 t *can-castle*))
    (format t "~a: ~a~%" move val)
    (setf *last-move* move)
    (move-piece *board* move 0)
    (print-board *board*)))

(defun human-move (fcol frow tcol trow)
  (let ((moves (list-moves *board* (can-castle-black *can-castle*) nil
			   (chess-move-double-jump *last-move*)
			   (chess-move-old-col *last-move*))))
    (loop for move in moves 
       when (and (= (chess-move-old-col move) fcol)
		 (= (chess-move-old-row move) frow)
		 (= (chess-move-new-col move) tcol)
		 (= (chess-move-new-row move) trow)) do
	 (setf *last-move* move)
	 (move-piece *board* move 7)
	 (print-board *board*))))

(new-game)
(print-board *board*)
(human-move 1 6 2 5)
(computer-move)

(get-internal-run-time 

(format t "~a~%" (list-moves *board* (can-castle-black *can-castle*) nil nil 0))
(format t "~a~%" (list-bishop-moves *board* 4 6 nil nil))

(defun missing-bishop ()
  (let ((board (make-array (list 8 8) :element-type '(signed-byte 8))))
    (loop for col in (list 0 1 2 5 6 7) do
    ;;(loop for col in (list 0 1 5 6 7) do
	 (setf (aref board col 1) (make-piece t :pawn)))
    (setf (aref board 4 2) (make-piece t :pawn))
    (setf (aref board 0 0) (make-piece t :rook))
    (setf (aref board 1 0) (make-piece t :knight))
    ;;(setf (aref board 2 0) (make-piece t :bishop))
    ;;(setf (aref board 1 4) (make-piece t :bishop))
    (setf (aref board 6 4) (make-piece t :queen))
    (setf (aref board 4 0) (make-piece t :king))
    (setf (aref board 7 0) (make-piece t :rook))
    ;;(setf (aref board 2 2) (make-piece t :knight)) original

    (setf (aref board 4 1) (make-piece t :knight))
    (setf (aref board 3 1) (make-piece t :bishop))
    (setf (aref board 2 5) (make-piece t :bishop))
    
    (loop for col in (list 0 1 5 6 7) do
	 (setf (aref board col 6) (make-piece nil :pawn)))
    ;;(setf (aref board 3 4) (make-piece nil :pawn))
    (setf (aref board 4 5) (make-piece nil :pawn))
    (setf (aref board 0 7) (make-piece nil :rook))
    (setf (aref board 6 7) (make-piece nil :rook))
    ;;(setf (aref board 3 7) (make-piece nil :queen))
    (setf (aref board 4 7) (make-piece nil :king))
    (setf (aref board 2 5) (make-piece nil :knight))
    (setf (aref board 5 5) (make-piece nil :knight))
    (setf (aref board 2 4) (make-piece nil :bishop))
    (setf (aref board 3 6) (make-piece nil :bishop))


    (setf (aref board 3 3) (make-piece nil :pawn))
    (setf (aref board 2 6) (make-piece nil :queen))
    
    (print-board board)

    (multiple-value-bind (val move)
	(time (negamaxit board *last-move* 7 -32500 32500 t *can-castle*))
	;;(time (negamaxit board *last-move* 7 -32500 32500 nil *can-castle*))
      (format t "~a: ~a~%" move val)
      ;;(setf *last-move* move)
      ;;(move-piece board move 0)
      (print-board board)))
    ))

(missing-bishop)

7|r| | |q|k| |r| |
6|p|p| |b| |p|p|p|
5| | |n| |p|n| | |
4| |B|b|p| | |Q| |
3| | | | | | | | |
2| | |N| |P| | | |
1|P|P|P| | |P|P|P|
0|R|N|B| |K| | |R|
  0 1 2 3 4 5 6 7 
