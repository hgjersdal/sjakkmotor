(in-package :sjakk)

(defparameter *board* (make-initial-board))
(defparameter *can-castle* (make-can-castle))
(defparameter *last-move* (make-chess-move))

(defun new-game ()
  (setf *board* (make-initial-board)
	*can-castle* (make-can-castle)
	*last-move* (make-chess-move)))
  
(defun computer-move ()
  (format t "Processing!~%")
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
