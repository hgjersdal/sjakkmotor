(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :sjakk3)
;;;Various data containers for describing a chess game.

(defstruct chess-move 
  (old-col 0 :type (unsigned-byte 8)) (old-row 0 :type (unsigned-byte 8))
  (new-col 0 :type (unsigned-byte 8)) (new-row 0 :type (unsigned-byte 8))
  (old-val 0 :type fixnum) (new-val 0 :type fixnum)
  (captured-val 0 :type fixnum) (evaluation 0 :type fixnum)
  (enpassant nil :type boolean) (double-jump nil :type boolean)
  (castle-short nil :type boolean) (castle-long nil :type boolean)
  (legalp t :type boolean))

(defparameter *max-eval* 32500)

(defun make-move-array ()
  (let ((ma (make-array 21 :element-type '(vector chess-move 256) :initial-element
			(make-array 256 :element-type 'chess-move :adjustable t
				    :fill-pointer 0))))
    (loop for index from 0 to 20 do
	 (setf (aref ma index) (make-array 256 :element-type 'chess-move
					   :adjustable t :fill-pointer 0))
	 (loop for i2 from 0 to 255 do
	      (setf (aref (aref ma index) i2) (make-chess-move))))
    ma))

(defstruct castlings
  (long t :type boolean) (short t :type boolean))

(defun make-initial-board ()
  (let ((board (make-array (list 8 8) :element-type 'fixnum :initial-element 0)))
    (dotimes (col 8)
      (setf (aref board col 1) 1)
      (setf (aref board col 6) -1))
    (dotimes (i 2)
      (let ((sign (if (= i 0) 1 -1))
	    (row (if (= i 0) 0 7)))
	(dotimes (col 8)
	  (let ((piece (aref #(4 2 3 5 6 3 2 4) col)))
	    (setf (aref board col row) (* sign piece))))))
    board))

(defun initial-played-moves ()
  (let ((array (make-array 256 :element-type 'chess-move
			   :initial-element (make-chess-move))))
    (loop for i from 0 to 255 do (setf (aref array i) (make-chess-move)))
    array))

(defclass chess-game ()
  ((board :accessor board :initform (make-initial-board)
	  :type (simple-array fixnum (8 8))
	  :initarg :board)
   (move-arrays :accessor move-arrays :initform (make-move-array))
   (white-castle :accessor white-castle :initform (make-castlings) :type castlings)
   (black-castle :accessor black-castle :initform (make-castlings) :type castlings)
   (played-moves :accessor played-moves :initform (initial-played-moves))
   (n-moves :accessor n-moves :initform 0)))

