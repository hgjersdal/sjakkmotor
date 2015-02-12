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
   (white-to-move :accessor white-to-move :initform t)
   (current :accessor current :initform t)
   (thinking :accessor thinking :initform nil)
   (n-moves :accessor n-moves :initform 0)
   (password :accessor password :initform "")
   (computer-game :accessor computer-game :initform (make-instance 'sjakk3::chess-game))
   (show-game :accessor show-game :initform (make-instance 'sjakk3::chess-game))))

(defparameter *games* (make-hash-table :test #'equalp)) 

(defun authenticatedp (widget)
  (with-slots (password) widget
    (cond 
      ((string= password "gratulerer-med-Ida") t)
      ((string= password "Mozart-Beethoven") t)
      ((string= password "sjakkmotortest") t)
      (t nil))))

(defun authenticate (widget)
  (when (authenticatedp widget)
    (let ((game (gethash (password widget) *games*)))
      (if (null game)
	  (setf (gethash (password widget) *games*) (computer-game widget))
	  (progn 
	    (setf (computer-game widget) game)
	    (setf (n-moves widget) (sjakk3::n-moves game))
	    (play-to-move (- (sjakk3::n-moves game) 1) widget))))))
		 
(defun push-moves-with-check (widget)
  (with-slots (show-game) widget
    (sjakk3::depth-search show-game 2 0 (evenp (sjakk3::n-moves show-game)) 0)
    (setf (moves widget) (aref (sjakk3::move-arrays show-game) 0))))

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

(defun background (widget col row attackedp)
  (let* ((n-moves (sjakk3::n-moves (show-game widget)))
	 (move (if (> n-moves 0)
		   (aref (sjakk3::played-moves (show-game widget)) (- n-moves 1))
		   nil)))
    (cond (attackedp "#ffc")
	  ((and move
		(= col (sjakk3::chess-move-old-col move))
		(= row (sjakk3::chess-move-old-row move))) "#ccf")
	  ((and move
		(= col (sjakk3::chess-move-new-col move))
		(= row (sjakk3::chess-move-new-row move))) "#ccf")
	  ((and (oddp row) (oddp col)) "#ccc")
	  ((and (evenp row) (evenp col)) "#ccc")
	  (t "#fff"))))

(defun cell-style (border-color)
  (format nil "border: 0px solid black; padding: 0px;  margin: 0px; background: ~a; height: 50px; width: 50px;font-size: 23pt"
	  border-color))

(defun norm-square (col row val widget)
  (with-html (:td :style (cell-style (background widget col row nil))
		  (str (format nil "~a" (chess-piece-to-html val))))))

(defun piece-square (col row val widget)
  (with-html (:td :style (cell-style (background widget col row nil))
		  (render-link
		   (f_% (setf (col widget) col)
			(setf (row widget) row)
			(mark-dirty widget))
		   (chess-piece-to-html val)))))

(defun play-to-move (move-n widget)
  (let ((sg (show-game widget))
	(cg (computer-game widget)))
    (if (= move-n (- (n-moves widget) 1))
	(setf (current widget) t)
	(setf (current widget) nil))
    (setf (sjakk3::board sg) (sjakk3::make-initial-board))
    (setf (sjakk3::n-moves sg) 0)
    (loop for move across (sjakk3::played-moves cg)
       for index from 0 to move-n
       when (evenp index) do
	 (sjakk3::move-piece sg (sjakk3::copy-chess-move move) (sjakk3::white-castle sg) 0)
       when (oddp index) do
	 (sjakk3::move-piece sg (sjakk3::copy-chess-move move) (sjakk3::black-castle sg) 7))))

(defparameter *sync-mutex* (bordeaux-threads:make-lock)) 

(defun sync-last-move (widget)
  (bordeaux-threads:with-lock-held (*sync-mutex*)
    (with-slots (computer-game show-game) widget
      (when (or (thinking widget)
		(= (n-moves widget) (sjakk3::n-moves computer-game)))
	(return-from sync-last-move))
      (let ((move (n-moves widget))
	    (current (current widget)))
	(setf (aref (sjakk3::played-moves show-game) move)
	      (sjakk3::copy-chess-move (aref (sjakk3::played-moves computer-game) move)))
	(when current (play-to-move move widget))
	(incf (n-moves widget))
	(setf (current widget) current)))))

(defun comp-move-thread (widget)
  (when (white-to-move widget) (return-from comp-move-thread))
  (let ((game (computer-game widget)))
    (if (< (sjakk3::n-moves game) 0)
	(bordeaux-threads:make-thread
	 (lambda ()
	   (setf (thinking widget) t)
	   (sjakk3::analyze game nil 6 3)
	   (sjakk3::random-move game nil
				(aref (sjakk3::move-arrays game) 0) 15)
	   (setf (thinking widget) nil)
	   (sync-last-move widget)
	   (setf (white-to-move widget) t)) :name "chess-eval")
	(bordeaux-threads:make-thread
	 (lambda ()
	   (let ((depth (cond
			  ((< (length (moves widget)) 15) 8)
			  ((< (length (moves widget)) 27) 7)
			  ((< (length (moves widget)) 37) 6)
			  (t 16))))
	     (setf (thinking widget) t)
	     (computer-move game nil depth 5 t)
	     (setf (thinking widget) nil)
	     (sync-last-move widget)
	     (setf (white-to-move widget) t)) :name "chess-eval")))))

(defun attacked-square (col row val widget)
  (with-html (:td :style (cell-style (background widget col row t))
		  (render-link
		   (f_%
		     (setf (white-to-move widget) nil)
		     (human-move (computer-game widget) t (col widget) (row widget) col row)
		     (sync-last-move widget)
		     (comp-move-thread widget)
		     (setf (col widget) -1)
		     (setf (row widget) -1)
		     (mark-dirty widget))
		   (if (= val 0)
		       (chess-piece-to-html (aref (sjakk3::board (show-game widget)) (col widget) (row widget)))
		       (chess-piece-to-html val))
		   :class (if (= val 0) "chesspiece" "")))))

(defun render-table-cell (widget col row)
  (let ((val (aref (sjakk3::board (show-game widget)) col row))
	(attacked (attacked-p (moves widget) (col widget) (row widget) col row)))
    (cond
      ((not (can-play widget)) (norm-square col row val widget))
      ((> val 0) (piece-square col row val widget))
      ((and attacked (current widget)) (attacked-square col row val widget))
      (t (norm-square col row val  widget)))))

(defun count-queens (widget)
    (loop for col from 0 to 7
       with count = 0 do
	 (loop for row from 0 to 7
	    when (= (abs (aref (sjakk3::board (computer-game widget)) col row)) 5) do
	      (incf count))
       finally (return count)))

(defun count-pieces (widget scale)
  (loop for col from 0 to 7
     with value = 0 do
       (loop for row from 0 to 7 do
	    (case (* scale (aref (sjakk3::board (computer-game widget)) col row))
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

(defun fancy-print-move (move widget)
  (when (>= move (n-moves widget))
    (return-from fancy-print-move))
  (let ((m (aref (sjakk3::played-moves (computer-game widget)) move)))
    (render-link (f_% (play-to-move move widget)
		      (mark-dirty widget))
		 (format nil "~a~a~a"
			 (chess-piece-to-html (sjakk3::chess-move-old-val m))
			 (aref "abcdefgh" (sjakk3::chess-move-new-col m))
			 (+ (sjakk3::chess-move-new-row m) 1)))))

(defun clickable-moves (widget)
  (with-html
    (:ol (loop for move from 0 to (- (n-moves widget) 1) by 2 do
	      (with-html (:li (fancy-print-move move widget)
			      (str " ")
			      (fancy-print-move (+ move 1) widget)))))))

(defmethod render-widget-body ((widget game) &rest args)
  (declare (ignorable args))
  (unless (authenticatedp widget)
    (with-html-form 
  	(:post (lambda (&key search-string &allow-other-keys)
  		 (setf (password widget) search-string)
  		 (authenticate widget)
  		 (mark-dirty widget))
  	       :id "searchbar")
      (:input :type "text" :name "search-string")
      (:input :class "button3" :type "submit" :value "Ask permission")))
  (if (thinking widget)
      (with-html  (:br) (str (format nil "Sjakkmotor is thinking... (~a sec)" (/ (sjakk3::search-time (computer-game widget)) 1000))))
      (with-html  (:br) "White to move!"))
  (if (thinking widget)
      (with-html
   	(:script :type "text/javascript"
		 (str (format nil 
			      "setTimeout(function(){ location.reload(); }, ~a);"
			      (min 10000 (+ 100 (sjakk3::search-time (computer-game widget))))))))
      (push-moves-with-check widget))
  (endgame-check widget)
  (with-html
    (:table :style "text-align: center; border-collapse: collapse; border-spacing: 0; border: 2px solid black;"
   	    (loop for row from 7 downto 0 do
   		 (with-html
		   (:tr
		    (loop for col from 0 to 7 do
			 (render-table-cell widget col row)))))))
  (let ((moves 
	 (loop for i from 0 below (n-moves widget)
	    collecting (sjakk3::chess-move-captured-val (aref (sjakk3::played-moves (show-game widget)) i)))))
    (mapcar
     (lambda (x) (with-html (str (format nil "~a" x))))
     (mapcar #'chess-piece-to-html (remove-if (lambda (x) (<= x 0)) (sort moves #'>))))
    (with-html (:br))
    (mapcar
     (lambda (x) (with-html (str (format nil "~a" x))))
     (mapcar #'chess-piece-to-html (remove-if (lambda (x) (>= x 0)) (sort moves #'<)))))
  (when (> (n-moves widget) 0)
    (clickable-moves widget))
  (let ((current-time (/ (sjakk3::search-time (computer-game widget)) 1000)))
    (when (authenticatedp widget)
      (with-html-form 
	  (:post (lambda (&key difficulty &allow-other-keys)
		   (setf (sjakk3::search-time (computer-game widget))
			 (* 1000 (read-from-string difficulty)))
		   (mark-dirty widget)) :id "searchbar")
	(:select :class "button3" :name "difficulty"
		 (:option :value current-time (str (format nil "~asec" current-time)))
		 (:option :value 1 "1sec")
		 (:option :value 5 "5sec")
		 (:option :value 10 "10sec")
		 (:option :value 60 "1min"))
	(:input :class "button3" :type "submit" :value "Set difficulty"))
      (with-html-form 
	  (:post (lambda (&key &allow-other-keys)
		   (setf (col widget) -1)
		   (setf (row widget) -1)
		   (setf (show-game widget) (make-instance 'sjakk3::chess-game))
		   (setf (computer-game widget) (make-instance 'sjakk3::chess-game))
		   (setf (sjakk3::search-time computer-game) current-time)
		   (setf (n-moves widget) 0)
		   (setf (white-to-move widget) t)
		   (setf (current widget) t)
		   (setf (gethash (password widget) *games*) (computer-game widget))
		   (mark-dirty widget)) :id "Abandoner")
	(:input :class "button3" :type "submit" :value "Restart match!")))))


;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (let ((cg (make-instance 'game)))
    (setf (widget-children root)
	  (list cg))))

;;(reset-sessions)
