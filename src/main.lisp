(defpackage :limine
  (:use :cl))

(defun generate-field (size mine-count)
  "Generate a random minefield given a width and a height of the field.
   Returns a representation of the minefield as a 2D array."
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
  "Check if a given cell position is valid on a given board"
  (let ((board-size (array-dimension board 0)))
    (and (>= (first cell) 0)
	 (< (first cell) board-size)
	 (>= (second cell) 0)
	 (< (second cell) board-size))))

(defun check-adjacent-cells (board cell)
  "Check adjacent cells for mines, returns the total mine count for this cell."
  (let ((dx '(-1 -1 -1 0 0 1 1 1))
	(dy '(-1 0 1 -1 1 -1 0 1))
	(mine-count 0))
    (loop for d from 0 below 8
	  do (let ((row (+ (first cell) (nth d dx)))
		   (col (+ (second cell) (nth d dy))))
	       (if (valid-cell-p `(,row ,col) board)
		   (if (aref board row col)
		       (setf mine-count (1+ mine-count))))))
    mine-count))

(defun generate-board (field)
  "Given an already generated minefield, add surrounding clues and return a board.")

(in-package :limine)

(defparameter *difficulties* '((beginner . (9 10))
			       (intermediate . (16 40))
			       (advanced . (24 99)))
  "LiMine difficulty alist.")

(defun main ()
  (let ((minefield (generate-field 5 6)))
    (print (format nil "Minefield: ~A" minefield))))
