(in-package :limine)
(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defparameter *tile-storage* '()
	      "Later gets populated by draw-minefield, contains buttons and their respective coordinates.")

(defparameter *styles* `(,(make-style cell-blue (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :blue)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-green (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :darkgreen)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-red (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :red)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-darkblue (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :darkblue)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-darkred (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :darkred)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-cyan (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :cyan)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-purple (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :purple)
                                      :font "Monospace 10 bold")
                         ,(make-style cell-grey (:extend "TButton")
                                      :foreground (nodgui.utils:make-tk-color :grey)
                                      :font "Monospace 10 bold")
			 ,(make-style cell-empty (:extend "TButton")
				      :overrelief :sunken
				      :relief :sunken))
  "List of all possible tile foregrounds.")

(defparameter *tile-colour-alist*
  '((0 . cell-empty)
    (1 . cell-blue)
    (2 . cell-green)
    (3 . cell-red)
    (4 . cell-darkblue)
    (5 . cell-darkred)
    (6 . cell-cyan)
    (7 . cell-purple)
    (8 . cell-grey)))


(defparameter *difficulties* '((beginner . (9 10))
                               (intermediate . (16 40))
                               (advanced . (24 99)))
  "limine difficulty alist.")

(defparameter *dx* '(-1 -1 -1 0 0 1 1 1)
	      "Adjacent tiles on the x axis")
(defparameter *dy* '(-1 0 1 -1 1 -1 0 1)
	      "Adjacent tiles on the y axis")

(defun prepare-styles ()
  (loop for style in *styles* do
	(apply-style style)))

(defun generate-field (size mine-count)
  "generate a random minefield given a width and a height of the field.
   returns a representation of the minefield as a 2d array."
  (let ((board (make-array `(,size ,size) :initial-element nil))
	(mine-positions '()))
    (loop while (< (length mine-positions) mine-count)
	  do (let ((row (random (1- size)))
		   (col (random (1- size))))
	       (if (not (member `(,row ,col) mine-positions))
		   (progn
		     (pushnew `(,row ,col) mine-positions)
		     (setf (aref board col row) t)))))
    board))

(defun valid-cell-p (cell board)
  "check if a given cell position is valid on a given board"
  (let ((board-size (array-dimension board 0)))
    (and (>= (first cell) 0)
	 (< (first cell) board-size)
	 (>= (second cell) 0)
	 (< (second cell) board-size))))

(defun check-adjacent-mines (board cell)
  "Check adjacent mines nearby the given cell.
   Returns the amount of mine neighbours."
  (let ((mine-count 0))
    (loop for d from 0 below 8
	  do (let ((row (+ (first cell) (nth d *dx*)))
		   (col (+ (second cell) (nth d *dy*))))
	       (if (valid-cell-p `(,row ,col) board)
		   (if (and (not (numberp (aref board row col)))
			    (aref board row col))
		       (setf mine-count (1+ mine-count))))))
    mine-count))

(defparameter *debug-call-deepness* 0
  "How deep a call is going.")

(defun check-blank-neighbours (field board cell)
  "Check adjacent blank tiles nearby the given cell. Recursive.
   Returns a bitmap of tiles that need to be opened"
  (setf *debug-call-deepness* (1+ *debug-call-deepness*))
  (let ((cells-nearby '()))
    (loop for d from 0 below 8
	  do (let ((row (+ (first cell) (nth d *dx*)))
		   (col (+ (second cell) (nth d *dy*))))
	       (if (valid-cell-p `(,row ,col) board)
		   (cond
		     ((not (aref *tile-storage* row col)) (format t "Cell (~a, ~a) already revealed..." row col)) ;; Tile was already revealed, skip.
		     ((numberp (aref board row col))
		      (if (aref *tile-storage* row col)
			  (cell-action field (aref *tile-storage* row col) `(,row ,col) board)))
		    ((format t "(~a,~a) is probably a mine" row col))))))))

(defun generate-board-with-diff (diff)
  "Generate a board with a selected difficulty"
  (let ((board (apply 'generate-field (cdr (assoc diff *difficulties*)))))
    (nested-loop (x y) (array-dimensions board)
	       (let ((mine (aref board x y)))
		 (if (not mine)
		     (setf (aref board x y) (check-adjacent-mines board `(,x ,y))))))
    board))

(defun cell-action (field button coords board)
  "Cell button action. Needs a board to work, lol."
  (setf *debug-call-deepness* (1+ *debug-call-deepness*))
  (format t "Running cell action for cell ~A" coords)
  (if (not button)
      nil)
  (let ((cell (aref board
		    (first coords)
		    (second coords))))
    (cond
      ((numberp cell) (progn
			(print "Hint.")
		       (if (not (= cell 0))
			   (progn
			     (let ((new-style (cdr (assoc cell *tile-colour-alist*))))
			       (setf (text button) (format nil "~a" cell))
			       (style-configure button new-style)))
			   (progn
			     (print "About to go into recursion!")
			     (print *debug-call-deepness*)
			     (setf (aref *tile-storage* (first coords) (second coords)) nil)
			     (print "Removed current tile from storage.")
			     (style-configure button (cdr (assoc 0 *tile-colour-alist*)))
			     (configure button :state "disabled")
			     (check-blank-neighbours field board coords)))))
     (cell (progn
	     (message-box "You lost :("
			  "Game Over"
			  "ok"
			  "warning"
			  :parent *tk*)
	     (exit-nodgui)))))
  (setf (aref *tile-storage* (first coords) (second coords)) nil))

(defun draw-minefield (board)
  "Draw the current minefield into the screen."
  (let ((field-frame (make-instance 'frame)))
    (grid field-frame 0 0
	  :padx 5
	  :pady 5)
    (setf *tile-storage* (make-array (array-dimensions board)))
    (nested-loop (x y) (array-dimensions board)
	       (let* ((stored-coords `(,x ,y))
		      (b (make-instance 'button
					:text " "
					:width 1
					:grid `(,x ,y)
					:master field-frame)))
		 (setf (aref *tile-storage* x y) b)
		 (setf (command b) (lambda ()
				     (setf *debug-call-deepness* 0)
				     (cell-action field-frame b stored-coords board)))))))
(defun main ()
  (let ((sample-board (generate-board-with-diff 'beginner)))
    (with-nodgui ()
		 (wm-title *tk* "Minesweper")
		 (prepare-styles)
		 (draw-minefield sample-board))))
