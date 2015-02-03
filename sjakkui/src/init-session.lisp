;;(ql:quickload "sjakkui")
(in-package :sjakkui)

(defun chess-piece-to-html (piece)
  (case piece
    (1 "&#9817;")
    (2 "&#9816;")
    (3 "&#9815;")
    (4 "&#9814;")
    (5 "&#9813;")
    (6 "&#9812;")
    (0 "") 
    (-1 "&#9823;")
    (-2 "&#9822;")
    (-3 "&#9821;")
    (-4 "&#9820;")
    (-5 "&#9819;")
    (-6 "&#9818;")))

(defwidget game ()
  ((col :accessor col :initform -1)
   (row :accessor row :initform -1)
   (moves :accessor moves :initform (make-array 256 :adjustable t :fill-pointer 0))
   (board :accessor board :initform (make-array '(8 8)))
   (white-to-move :accessor white-to-move :initform t)))

(defun copy-board (widget)
  (let ((b (board widget)))
    (loop for col from 0 to 7 do
	 (loop for row from 0 to 7 do
	      (setf (aref b col row) (aref (sjakk3::board *chess-game*) col row))))))

(defparameter *chess-game* (make-instance 'sjakk3::chess-game))

(defun push-moves-with-check (widget)
  (let ((move (if (= (sjakk3::n-moves *chess-game*) 0)
		  (sjakk3::make-chess-move)
		  (aref (sjakk3::played-moves *chess-game*) (- (sjakk3::n-moves *chess-game*) 1)))))
    (sjakk3::negamaxit *chess-game* 2 move -32500 32500 1
		       (sjakk3::white-castle *chess-game*) (sjakk3::black-castle *chess-game*)
		       0 0 0)
    (setf (moves widget) (aref (sjakk3::move-arrays *chess-game*) 0))))


(defun attacked-p (moves oc or nc nr)
  (loop for move across moves
     when (and
	   (sjakk3::chess-move-legalp move)
	   (= oc (sjakk3::chess-move-old-col move))
	   (= or (sjakk3::chess-move-old-row move))
	   (= nc (sjakk3::chess-move-new-col move))
	   (= nr (sjakk3::chess-move-new-row move)))
     do (return t)
     finally (return nil)))

(defun background (widget col row)
  (let* ((n-moves (sjakk3::n-moves *chess-game*))
	 (move (if (> n-moves 0) (aref (sjakk3::played-moves *chess-game*) (- n-moves 1)) nil)))
    (cond ((and (white-to-move widget) move
		(= col (sjakk3::chess-move-old-col move))
		(= row (sjakk3::chess-move-old-row move)))
	   "#ccf")
	  ((and (white-to-move widget) move
		(= col (sjakk3::chess-move-new-col move))
		(= row (sjakk3::chess-move-new-row move)))
	   "#ccf")
	  ((or (and (oddp row) (oddp col)) (and (evenp row) (evenp col))) "#ccc")
	  (t "#fff"))))

(defun norm-square (col row val border widget)
  (with-html 
    (:td :style (format nil "border: 1px solid ~a; background: ~a; height: 80px; width: 80px;font-size: 50pt"
			border (background widget col row))
	 (str (format nil "~a" (chess-piece-to-html val))))))

(defun piece-square (col row val widget)
  (with-html 
    (:td :style (format nil "border: 1px solid ~a; background: ~a; height: 80px; width: 80px;font-size: 50pt;};"
			"black" (background widget col row))
	 (render-link
	  (f_% (setf (col widget) col)
	       (setf (row widget) row)
	       (mark-dirty widget))
	  (chess-piece-to-html val)))))

(defun attacked-square (col row val widget)
  (with-html 
    (:td :style (format nil "border : 1px solid ~a; background: ~a; height: 80px; width: 80px;font-size: 50pt;};"
			"yellow" (background widget col row))
	 (render-link
	  (f_%
	    (setf (white-to-move widget) nil)
	    (human-move *chess-game* t (col widget) (row widget) col row)
	    (copy-board widget)
	    (bordeaux-threads:make-thread
	     (lambda ()
	       (let ((depth (cond
			      ((< (length (moves widget)) 15) 7)
			      ((< (length (moves widget)) 30) 6)
			      (t 5))))
		 (computer-move *chess-game* nil depth 5 t))
	       (copy-board widget)
	       (setf (white-to-move widget) t)) :name "chess-eval")
	    (setf (col widget) -1)
	    (setf (row widget) -1)
	    (mark-dirty widget))
	  (if (= val 0)
	      (chess-piece-to-html (aref (board widget) (col widget) (row widget)))
	      (chess-piece-to-html val))
	  :class (if (= val 0) "chesspiece" "")))))

(defun render-table-cell (widget col row)
  (let ((val (aref (board widget) col row))
	(attacked (attacked-p (moves widget) (col widget) (row widget) col row)))
    (cond
      ((not (white-to-move widget)) (norm-square col row val "black" widget))
      ((> val 0) (piece-square col row val widget))
      (attacked (attacked-square col row val widget))
      (t (norm-square col row val "black" widget)))))
    
(defmethod render-widget-body ((widget game) &rest args)
  (declare (ignorable args))
  (if (white-to-move widget)
      (push-moves-with-check widget)
      (with-html
	(:script :type "text/javascript"
		 "setTimeout(function(){ location.reload(); }, 4000);")))
  (with-html
    (:table :style "text-align: center;"
	    (loop for row from 7 downto 0 do
		 (with-html
		   (:tr
		    (loop for col from 0 to 7 do
			 (render-table-cell widget col row))))))
    (render-link (f_% (setf (col widget) -1)
		      (setf (row widget) -1)
		      (setf *chess-game* (make-instance 'sjakk3::chess-game))
		      (setf (white-to-move widget) t)
		      (push-moves-with-check widget)
		      (copy-board widget)
		      (mark-dirty widget))
		 "Reset!")))

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (let ((cg (make-instance 'game)))
    (copy-board cg)
    (setf (widget-children root)
	  (list cg))))
