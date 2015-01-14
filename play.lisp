(in-package :sjakk)

(defparameter *board* (make-initial-board))
(defparameter *can-castle* (make-can-castle))
(defparameter *last-move* (make-chess-move))

(defun new-game ()
  (setf *board* (make-initial-board)
	*can-castle* (make-can-castle)
	*last-move* (make-chess-move)))

(setf *last-move* (make-move 0 0 0 1 0 0 0 nil nil))

(defun computer-move (whitep)
  (format t "Processing!~%")
  (multiple-value-bind (val move)
      (time (negamaxit *board* *last-move* 7 -32500 32500 whitep *can-castle*))
    (format t "~a: ~a~%" move val)
    (setf *last-move* move)
    (move-piece *board* move 0)
    (print-board *board*)))

(defun human-move (fcol frow tcol trow whitep)
  (let ((moves (list-moves *board*
			   (if whitep (can-castle-white *can-castle*)
			       (can-castle-black *can-castle*))
			   whitep (chess-move-double-jump *last-move*)
			   (chess-move-old-col *last-move*))))
    (loop for move in moves 
       when (and (= (chess-move-old-col move) fcol)
		 (= (chess-move-old-row move) frow)
		 (= (chess-move-new-col move) tcol)
		 (= (chess-move-new-row move) trow)) do
	 (setf *last-move* move)
	 (move-piece *board* move (if whitep 0 7))
	 (print-board *board*))))

(new-game)
(print-board *board*)
(human-move 5 1 5 6 nil)
(computer-move t)
