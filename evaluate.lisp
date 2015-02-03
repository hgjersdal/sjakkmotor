(declaim (optimize (speed 3) (debug 0) (safety 0)))
(in-package :sjakk3)

(let ((piece-val (make-array 7 :element-type 'fixnum :initial-contents #(0 100 320 330 500 900 2000))))
  (defun piece-value (piece)
    (declare (type fixnum piece))
    (declare (type (simple-array fixnum (7)) piece-val))
    (the fixnum
	 (aref piece-val (abs piece)))))

(defparameter *piece-square-array*
  (make-array 6 :element-type '(simple-array fixnum (8 8))
	      :initial-element (make-array '(8 8) :element-type 'fixnum)))

(defparameter *king-not-endgame*
  (make-array '(8 8) :element-type 'fixnum :initial-contents
	      #(#(-30 -40 -40 -50 -50 -40 -40 -30 )
		#(-30 -40 -40 -50 -50 -40 -40 -30 )
		#(-30 -40 -40 -50 -50 -40 -40 -30 )
		#(-30 -40 -40 -50 -50 -40 -40 -30 )
		#(-20 -30 -30 -40 -40 -30 -30 -20 )
		#(-10 -20 -20 -20 -20 -20 -20 -10 )
		#( 20  20   0   0   0   0  20  20 )
		#( 20  30  10   0   0  10  30  20))))

(defparameter *king-endgame*
  (make-array '(8 8) :element-type 'fixnum :initial-contents
	      #(#(-50 -40 -30 -20 -20 -30 -40 -50 )
		#(-30 -20 -10   0   0 -10 -20 -30 )
		#(-30 -10  20  30  30  20 -10 -30 )
		#(-30 -10  30  40  40  30 -10 -30 )
		#(-30 -10  30  40  40  30 -10 -30 )
		#(-30 -10  20  30  30  20 -10 -30 )
		#(-30 -30   0   0   0   0 -30 -30 )
		#(-50 -30 -30 -30 -30 -30 -30 -50))))

(setf (aref *piece-square-array* 0)
      (make-array '(8 8) :element-type 'fixnum :initial-contents
		  #(#(0   0   0   0   0   0   0   0)
		    #(50  50  50  50  50  50  50  50 )
		    #(10  10  20  30  30  20  10  10 )
		    #( 5   5  10  25  25  10   5   5 )
		    #( 0   0   0  20  20   0   0   0 )
		    #( 5  -5 -10   0   0 -10  -5   5 )
		    #( 5  10  10 -20 -20  10  10   5 )
		    #( 0   0   0   0   0   0   0   0))))

(setf (aref *piece-square-array* 1)
      (make-array '(8 8) :element-type 'fixnum :initial-contents
		  #(#(-50 -40 -30 -30 -30 -30 -40 -50 )
		    #(-40 -20   0   0   0   0 -20 -40 )
		    #(-30   0  10  15  15  10   0 -30 )
		    #(-30   5  15  20  20  15   5 -30 )
		    #(-30   0  15  20  20  15   0 -30 )
		    #(-30   5  10  15  15  10   5 -30 )
		    #(-40 -20   0   5   5   0 -20 -40 )
		    #(-50 -40 -30 -30 -30 -30 -40 -50 ))))

(setf (aref *piece-square-array* 2)
      (make-array '(8 8) :element-type 'fixnum :initial-contents
		  #(#(-20 -10 -10 -10 -10 -10 -10 -20 )
		    #(-10   0   0   0   0   0   0 -10 )
		    #(-10   0   5  10  10   5   0 -10 )
		    #(-10   5   5  10  10   5   5 -10 )
		    #(-10   0  10  10  10  10   0 -10 )
		    #(-10  10  10  10  10  10  10 -10 )
		    #(-10   5   0   0   0   0   5 -10 )
		    #(-20 -10 -10 -10 -10 -10 -10 -20 ))))

(setf (aref *piece-square-array* 3)
      (make-array '(8 8) :element-type 'fixnum :initial-contents
		  #(#(  0   0   0   0   0   0   0   0 )
		    #(  5  10  10  10  10  10  10   5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #(  0   0   0   5   5   0   0   0))))

(setf (aref *piece-square-array* 4)
      (make-array '(8 8) :element-type 'fixnum :initial-contents
		  #(#(-20 -10 -10  -5  -5 -10 -10 -20 )
		    #(-10   0   0   0   0   0   0 -10 )
		    #(-10   0   5   5   5   5   0 -10 )
		    #( -5   0   5   5   5   5   0  -5 )
		    #(  0   0   5   5   5   5   0  -5 )
		    #(-10   5   5   5   5   5   0 -10 )
		    #(-10   0   5   0   0   0   0 -10 )
		    #(-20 -10 -10  -5  -5 -10 -10 -20))))

(setf (aref *piece-square-array* 5) *king-not-endgame*)

(defun endgame ()
  (setf (aref *piece-square-array* 5) *king-endgame*))

(defun not-endgame ()
  (setf (aref *piece-square-array* 5) *king-not-endgame*))

(defun square-val (bval col row)
  (declare (type fixnum bval)
	   (type fixnum col row)
	   (type (vector (simple-array fixnum (8 8)) 6) *piece-square-array*))
  (when (> bval 0)
    (setf row (- 7 row)
	  col (- 7 col)))
  (let ((a (aref *piece-square-array* (- (abs bval) 1))))
    (declare (type (simple-array fixnum (8 8)) a))
    (the fixnum (aref a row col))))

(defun evaluate (game)
  (declare (type chess-game game))
  (let ((value 0)
	(board (board game)))
    (declare (type (simple-array fixnum (8 8)) board)
	     (type fixnum value))
    (loop for col of-type integer from 0 to 7 do
	 (loop for row of-type integer from 0 to 7 do
	      (let* ((bval (aref board col row))
		     (scale (if (> bval 0) 1 -1)))
		(unless (= 0 bval)
		  (incf value (the fixnum (* scale (piece-value bval))))
		  (incf value (* scale (square-val bval col row)))))))
    (the fixnum value)))
