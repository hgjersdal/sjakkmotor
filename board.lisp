;;(declaim (optimize (speed 3) (debug 0) (safety 0)))

(in-package :sjakk)

(defun piece-type-to-val (type)
  (the (signed-byte 8)
       (ecase type (:pawn 1) (:knight 2) (:bishop 3)
	      (:rook 4) (:queen 5) (:king 6))))

(defun make-piece (whitep type)
  (the (signed-byte 8)
       (if whitep (piece-type-to-val type) (- (piece-type-to-val type)))))
    
(defun piece-whitep (piece)
  (> 0 piece))

(defun piece-type (piece)
  (abs piece))

(defun piece-letter (piece)
  (aref " PNBRQK" (piece-type piece)))

(defun print-piece (piece)
  (format t "~a ~a~%" (if (piece-whitep piece) "White" "Black")
	  (piece-letter piece)))

(defstruct can-castle 
  (white (cons t t) :type cons)
  (black (cons t t) :type cons))

(defun copy-castle (can-castle)
  (let ((white (can-castle-white can-castle))
	(black (can-castle-white can-castle)))
    (make-can-castle :white (cons (car white) (cdr white))
		     :black (cons (car black) (cdr black)))))
			   
(defun make-initial-board ()
  (let ((board (make-array (list 8 8) :element-type '(signed-byte 8) :initial-element 0)))
    (dotimes (col 8)
      (setf (aref board col 1) (make-piece t :pawn))
      (setf (aref board col 6) (make-piece nil :pawn)))
    (dotimes (i 2)
      (let ((whitep (if (= i 0) t nil))
	    (row (if (= i 0) 0 7)))
	(dotimes (col 8)
	  (let ((piece (aref #(:rook :knight :bishop :queen :king :bishop :knight :rook) col)))
	    (setf (aref board col row) (make-piece whitep piece))))))
    board))

(defun print-board (board)
  (format t "~%")
  (loop for row from 7 downto 0 do
       (format t "~a ~a|" row (+ row 1))
       (loop for col from 0 to 7 do
	    (if (= (aref board col row) 0)
		(format t " |")
		(format t "~a|" (if (piece-whitep (aref board col row))
				    (char-downcase (piece-letter (aref board col row)))
				    (char-upcase (piece-letter (aref board col row)))))))
       (format t "~%"))
  ;;  (format t "  a b c d e f g h ~%"))
  (format t "    a b c d e f g h ~%")
  (format t "    0 1 2 3 4 5 6 7 ~%")
  )

