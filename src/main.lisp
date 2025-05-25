(in-package :limine)

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

(defun check-adjacent-cells (board cell)
  "check adjacent cells for mines, returns the total mine count for this cell."
  (let ((dx '(-1 -1 -1 0 0 1 1 1))
	(dy '(-1 0 1 -1 1 -1 0 1))
	(mine-count 0))
    (loop for d from 0 below 8
	  do (let ((row (+ (first cell) (nth d dx)))
		   (col (+ (second cell) (nth d dy))))
	       (if (valid-cell-p `(,row ,col) board)
		   (if (and (not (numberp (aref board row col))) (aref board row col))
		       (setf mine-count (1+ mine-count))))))
    mine-count))

(defun generate-board-with-diff (diff)
  "Generate a board with a selected difficulty"
  (let ((board (apply 'generate-field (cdr (assoc diff *difficulties*)))))
    (nested-loop (x y) (array-dimensions board)
	       (let ((mine (aref board x y)))
		 (if (not mine)
		     (setf (aref board x y) (check-adjacent-cells board `(,x ,y))))))
    board))

(defparameter *difficulties* '((beginner . (9 10))
			       (intermediate . (16 40))
			       (advanced . (24 99)))
	      "limine difficulty alist.")

(defun cell-action (button coords board)
  "Cell button action. Needs a board to work, lol."
  (let ((cell (aref board
		    (first coords)
		    (second coords))))
    (cond
     ((numberp cell) (progn
		       (if (not (= cell 0))
			   ;; [TODO] Find other 0 cells nearby and recursively trigger them
			   (setf (text button) (format nil "~a" cell)))
		       (configure button :state :pressed)))
     (cell (progn
	     (message-box "You lost :("
			"Game Over"
			"ok"
			"warning"
			:parent *tk*)
	     (setf *exit-mainloop* t))))))

(defun draw-minefield (board)
  "Draw the current minefield into the screen."
  (let ((field-frame (make-instance 'frame)))
    (grid field-frame 0 0
	  :padx 5
	  :pady 5)
    (nested-loop (x y) (array-dimensions board)
	       (let* ((stored-coords `(,x ,y))
		      (b (make-instance 'button
					:text " "
					:width 1
					:master field-frame)))
		 (setf (command b) (lambda ()
				     (cell-action b stored-coords board)))
		 (grid b x y)))))
(defun main ()
  (let ((sample-board (generate-board-with-diff 'intermediate)))
    (with-ltk ()
	      (wm-title *tk* "Minesweper")
	      (draw-minefield sample-board))))
