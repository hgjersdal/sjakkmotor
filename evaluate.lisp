(declaim (optimize (speed 3) (debug 0) (safety 0)))

(in-package :sjakk)

(defun piece-value (piece)
  (the (unsigned-byte 16)
       (aref #(0 100 320 330 500 900 2000) (piece-type piece))))

(defparameter *piece-square-table* (make-hash-table))

(setf (gethash 1 *piece-square-table*)
      (make-array '(8 8) :element-type '(signed-byte 8) :initial-contents
		  #(#(0   0   0   0   0   0   0   0)
		    #(50  50  50  50  50  50  50  50 )
		    #(10  10  20  30  30  20  10  10 )
		    #( 5   5  10  25  25  10   5   5 )
		    #( 0   0   0  20  20   0   0   0 )
		    #( 5  -5 -10   0   0 -10  -5   5 )
		    #( 5  10  10 -20 -20  10  10   5 )
		    #( 0   0   0   0   0   0   0   0))))

(setf (gethash 2 *piece-square-table*)
      (make-array '(8 8) :element-type '(signed-byte 8) :initial-contents
		  #(#(-50 -40 -30 -30 -30 -30 -40 -50 )
		    #(-40 -20   0   0   0   0 -20 -40 )
		    #(-30   0  10  15  15  10   0 -30 )
		    #(-30   5  15  20  20  15   5 -30 )
		    #(-30   0  15  20  20  15   0 -30 )
		    #(-30   5  10  15  15  10   5 -30 )
		    #(-40 -20   0   5   5   0 -20 -40 )
		    #(-50 -40 -30 -30 -30 -30 -40 -50 ))))

(setf (gethash 3 *piece-square-table*)
      (make-array '(8 8) :element-type '(signed-byte 8) :initial-contents
		  #(#(-20 -10 -10 -10 -10 -10 -10 -20 )
		    #(-10   0   0   0   0   0   0 -10 )
		    #(-10   0   5  10  10   5   0 -10 )
		    #(-10   5   5  10  10   5   5 -10 )
		    #(-10   0  10  10  10  10   0 -10 )
		    #(-10  10  10  10  10  10  10 -10 )
		    #(-10   5   0   0   0   0   5 -10 )
		    #(-20 -10 -10 -10 -10 -10 -10 -20 ))))

(setf (gethash 4 *piece-square-table*)
      (make-array '(8 8) :element-type '(signed-byte 8) :initial-contents
		  #(#(  0   0   0   0   0   0   0   0 )
		    #(  5  10  10  10  10  10  10   5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #( -5   0   0   0   0   0   0  -5 )
		    #(  0   0   0   5   5   0   0   0))))

(setf (gethash 5 *piece-square-table*)
      (make-array '(8 8) :element-type '(signed-byte 8) :initial-contents
		  #(#(-20 -10 -10  -5  -5 -10 -10 -20 )
		    #(-10   0   0   0   0   0   0 -10 )
		    #(-10   0   5   5   5   5   0 -10 )
		    #( -5   0   5   5   5   5   0  -5 )
		    #(  0   0   5   5   5   5   0  -5 )
		    #(-10   5   5   5   5   5   0 -10 )
		    #(-10   0   5   0   0   0   0 -10 )
		    #(-20 -10 -10  -5  -5 -10 -10 -20))))

(setf (gethash 6 *piece-square-table*)
      (make-array '(8 8) :element-type '(signed-byte 8) :initial-contents
		  #(#(-30 -40 -40 -50 -50 -40 -40 -30 )
		    #(-30 -40 -40 -50 -50 -40 -40 -30 )
		    #(-30 -40 -40 -50 -50 -40 -40 -30 )
		    #(-30 -40 -40 -50 -50 -40 -40 -30 )
		    #(-20 -30 -30 -40 -40 -30 -30 -20 )
		    #(-10 -20 -20 -20 -20 -20 -20 -10 )
		    #( 20  20   0   0   0   0  20  20 )
		    #( 20  30  10   0   0  10  30  20))))

(defun square-val (bval col row)
  (aref (gethash (abs bval) *piece-square-table*)
	(if (< bval 0) row (- 7 row))
	(if (< bval 0) col (- 7 col))))

(defun evaluate (board)
  (let ((value 0))
    (loop for col from 0 to 7 do
	 (loop for row from 0 to 7 do
	      (let* ((bval (aref board col row))
		     (scale (if (> bval 0) 1 -1)))
		(incf value (* scale (piece-value bval)))
		(unless (= 0 bval)
		  (incf value (* scale (square-val bval col row)))))))
    value))
