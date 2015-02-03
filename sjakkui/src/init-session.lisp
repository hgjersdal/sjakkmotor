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
   (white-to-move :accessor white-to-move :initform t)
   (password :accessor password :initform "")))

(defun authenticatedp (widget)
  (string= (password widget) "gratulerer-med-Ida"))

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

(defun can-play (widget)
  (and (authenticatedp widget)
       (white-to-move widget)))

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

(defun comp-move-thread (widget)
  (if (< (sjakk3::n-moves *chess-game*) 5)
      (bordeaux-threads:make-thread
       (lambda ()
	 (sjakk3::analyze *chess-game* nil 5 3)
	 (sjakk3::random-move *chess-game* nil
			      (aref (sjakk3::move-arrays *chess-game*) 0) 15)
	 (copy-board widget)
	 (setf (white-to-move widget) t)) :name "chess-eval")
      (bordeaux-threads:make-thread
       (lambda ()
	 (let ((depth (cond
			((< (length (moves widget)) 15) 7)
			((< (length (moves widget)) 30) 6)
			(t 5))))
	   (computer-move *chess-game* nil depth 5 t))
	 (copy-board widget)
	 (setf (white-to-move widget) t)) :name "chess-eval")))

(defun attacked-square (col row val widget)
  (with-html 
    (:td :style (format nil "border : 1px solid ~a; background: ~a; height: 80px; width: 80px;font-size: 50pt;};"
			"yellow" (background widget col row))
	 (render-link
	  (f_%
	    (setf (white-to-move widget) nil)
	    (human-move *chess-game* t (col widget) (row widget) col row)
	    (copy-board widget)
	    (comp-move-thread widget)
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
      ((not (can-play widget)) (norm-square col row val "black" widget))
      ((> val 0) (piece-square col row val widget))
      (attacked (attacked-square col row val widget))
      (t (norm-square col row val "black" widget)))))

(defun count-queens (widget)
    (loop for col from 0 to 7
       with count = 0 do
	 (loop for row from 0 to 7
	    when (= (abs (aref (board widget) col row)) 5) do
	      (incf count))
       finally (return count)))

(defun count-pieces (widget scale)
  (loop for col from 0 to 7
     with value = 0 do
       (loop for row from 0 to 7 do
	    (case (* scale (aref (board widget) col row))
	      (5 (incf value 9))
	      (4 (incf value 5))
	      (3 (incf value 3))
	      (2 (incf value 3))))
     finally (return value)))

(defun endgame-check (widget)
  (let ((white (count-pieces widget 1))
	(black (count-pieces widget -1)))
    (if (or (= (count-queens widget) 0)
	    (and (<= white 12) (<= black 12)))
	(sjakk3::endgame)
	(sjakk3::not-endgame))))

(defmethod render-widget-body ((widget game) &rest args)
  (declare (ignorable args))
  (unless (authenticatedp widget)
    (with-html-form 
	(:post (lambda (&key search-string &allow-other-keys)
		 (setf (password widget) search-string)
		 (mark-dirty widget))
	       :id "searchbar")
      (:input :type "text" :name "search-string")
      (:input :class "button3" :type "submit" :value "Ask permission")))
  (if (white-to-move widget)
      (push-moves-with-check widget)
      (with-html
	(:script :type "text/javascript"
		 "setTimeout(function(){ location.reload(); }, 4000);")))
  (endgame-check widget)
  (with-html
    (:table :style "text-align: center;"
	    (loop for row from 7 downto 0 do
		 (with-html
		   (:tr
		    (loop for col from 0 to 7 do
			 (render-table-cell widget col row))))))
    (if (can-play widget)
	(render-link (f_% (setf (col widget) -1)
			  (setf (row widget) -1)
			  (setf *chess-game* (make-instance 'sjakk3::chess-game))
			  (setf (white-to-move widget) t)
			  (push-moves-with-check widget)
			  (copy-board widget)
			  (mark-dirty widget))
		     "Reset!")
	(render-link (f_% (copy-board widget)
			  (mark-dirty widget))
		     "Update!"))))

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (let ((cg (make-instance 'game)))
    (copy-board cg)
    (setf (widget-children root)
	  (list cg))))
