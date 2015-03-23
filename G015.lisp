;Bruno Alexandre Pires Henriques
;72913

;custom commands snippets
;(resolve-problema (make-array '(4 4) :initial-contents '((t nil nil nil) (nil nil t nil) (nil t nil nil) (nil nil nil t))) "a*")
;(make-array '(4 4) :initial-contents '((t nil nil nil) (nil nil t nil) (nil t nil nil) (nil nil nil t))

;============================================
;================ QUEEN STATE ===============
;============================================


(defstruct queens-state board-size number-placed positions ocupied-lines ocupied-columns)

(defun make-copy-queens-state (state)
	(make-queens-state 	:board-size (queens-state-board-size state) 
						:number-placed (queens-state-number-placed state)
					 	:positions (copy-list (queens-state-positions state))
					 	:ocupied-lines (copy-array (queens-state-ocupied-lines state))
					 	:ocupied-columns (copy-array (queens-state-ocupied-columns state))))

(defun put-queen! (state lin col)
	(let ((positions (queens-state-positions state)))
		;(print '@@@@@@@@@@@@@@BEFORE)
		;(print state)

		(setf (queens-state-positions state) (append positions (list (cons lin col))))
		(setf (aref (queens-state-ocupied-lines state) lin) t)
		(setf (aref (queens-state-ocupied-columns state) col) t)
		(incf (queens-state-number-placed state))

		;(print '@@@@@@@@@@@@@@AFTER)
		;(print state)
		))

(defun result-of-move (state lin pos)
	(let ((state-copy (make-copy-queens-state state)))
			(put-queen! state-copy lin pos)
			state-copy))

(defun free-line? (state line)
	;(format t "free-line? ~D~%" line) 
	(null (aref (queens-state-ocupied-lines state) line)))

(defun free-column? (state column)
	;(format t "free-column? ~D" column)
	(null (aref (queens-state-ocupied-columns state) column)))

(defun free-diagonal? (state line column)
	;(format t "free-diagonal? ~D ~D~%" line column)
	(let ((result t))
		(dolist (pos (queens-state-positions state))
			(let ((l (car pos))
				  (c (cdr pos)))

				;(format t "|dif lines| = ~D  == |dif cols| = ~D" (abs (- l line)) (abs (- c column)))
				(when (= (abs (- l line))
			 	 	     (abs (- c column)))
			 	 	  (progn
	 	 			 	(setf result nil) 
	 	 			 	(return)))))
			 	 	;(format t "trying next position"))))
		;(print result)
		result))

; Name: resolve-problema
; Arguments: initial-state, strategy
; Return: Bi-dimensional array with the queens position; nil if there is no solution
; Note:
(defun resolve-problema (initial-state strategy)
	(let* ((initial-state-transformed (convert-board-to-queens-state initial-state))
		   (result-state nil)
		   (transformed-result nil))

		;(print '---------------STARTINGSEARCH--------------)
		;(print initial-state-transformed)
		;(print '--------------------------------------------)
		

		(setf result-state (procura (cria-problema initial-state-transformed (list #'operator) :objectivo? #'objective? :heuristica #'heuristic) strategy))
		
		;(print '-------SEARCHFINISHED-------------)
		;(print result-state)
		(setf result-state (first (last (nth (- (length result-state) 4) result-state))))

		(when (not (null result-state))
			  (setf transformed-result (convert-queens-state-to-board result-state)))

		transformed-result))
		
(defun objective? (state)
	(= (queens-state-number-placed state) (queens-state-board-size state)))

(defun heuristic (state)
	(setf state state)
	1)


(defun transpose (x) 
	(let ((result (list))) 
		(dolist (p x) 
			(setf result (append result (list (cons (car (cdr p)) (car p)))))) 
		result))

(defun rotate-left (x board-size) 
	(let ((result (list))
		  (pivot (- board-size 1))) 
		; tranpose and flip horizontaly the line
		(dolist (p x)
			(let ((lin (car p))
				  (col (car (cdr p))))
			(setf result (append result (list (list (- pivot col) lin))))))
		result))

(defun operator (state)
	(let* ((size (queens-state-board-size state))
		   (sucessors (list))
		   (positions (list)))

		;(print '@@@@-FROM-THIS-STATE)
		;(print state)

		; only generate half of the lines*col combinations because the results are simmetrical
		(dotimes (l size)
			(if (free-line? state l)
				(dotimes (c size)
					;(print 'here2)
					;(print state)
					;(format t "lin ~D  col ~D --- v: ~D ~%" l c (< c (- size l)))
					(when (and (free-column? state c) (free-diagonal? state l c) )
						  (progn
						  		(setf positions nil) 
						  		(setf sucessors (append sucessors (list (result-of-move state l c)))))))))
				;(print 'skipping-line)))
				


		;(print 'generated)
		;(print sucessors)
		;(print (length sucessors))
		
		sucessors))

(defun free? (state line column)
	(and (free-line? state line) (free-column? state column) (free-diagonal? state line column)))

; =================================
; ======== CONVERTERS =============
; =================================

; Name: compact-matrix
; Arguments: ainitial-state, strategy
; Return: Bi-dimensional array with the queens position; nil if there is no solution
; Note: 
(defun convert-board-to-queens-state(matrix)
	(let* ((size (array-dimension matrix 0))
		   (positions (list))
		   (placed-queens 0)
		   (oc-lines (make-array size))
		   (oc-cols (make-array size)))

		(dotimes (line size)
			(dotimes (column size)
				(when (eq (aref matrix line column) t)
					(cons (cons line column) positions)
					(incf placed-queens)
					(setf (aref oc-lines line) t)
					(setf (aref oc-cols column) t)
					(return))))
		(make-queens-state :board-size size 
						   :number-placed placed-queens
					 	   :positions positions
					 	   :ocupied-lines oc-lines
					 	   :ocupied-columns oc-cols)))

(defun convert-queens-state-to-board(queen-state)
	(let* ((positions (queens-state-positions queen-state))
		   (size (queens-state-board-size queen-state))
		   (result-matrix (make-array (list size size))))
		(dolist (pos positions)
			(setf (aref result-matrix (car pos) (cdr pos)) t))
		result-matrix))


