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

(defun empty-queens-state (size)
	(make-queens-state 	:board-size size 
						:number-placed 0
					 	:positions (list)
					 	:ocupied-lines (make-array size)
					 	:ocupied-columns (make-array size)))

(defun put-queen! (state lin col)
	(let ((positions (queens-state-positions state)))
		;(print '@@@@@@@@@@@@@@BEFORE)
		;(print state)

		(setf (queens-state-positions state) (append positions (list (create-position lin col))))
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
		(dolist (pos (queens-state-positions state))
			(let ((l (first pos))
				  (c (second pos)))
				;(format t "|dif lines| = ~D  == |dif cols| = ~D" (abs (- l line)) (abs (- c column)))
				(when (= (abs (- l line))
			 	 	     (abs (- c column)))
			 	 	  (return-from free-diagonal? nil))))
			 	 	;(format t "trying next position"))))
		;(print result)
		t)

(defun position-x (pos)
	(first pos))

(defun position-y (pos)
	(second pos))

(defun create-position (lin col)
	(list lin col))

(defun equal-positions (pos1 pos2)
	(and (= (position-x pos1) 
			(position-x pos2))
		 (= (position-y pos1) 
		    (position-y pos2))))

(defun convert-board-to-queens-state(matrix)
	(let* ((size (array-dimension matrix 0))
		   (state (empty-queens-state size)))
		(dotimes (line size)
			(dotimes (column size)
				(when (eq (aref matrix line column) t)
					(put-queen! state line column)
					(return))))
		state))

(defun convert-queens-state-to-board(queen-state)
	(let* ((positions (queens-state-positions queen-state))
		   (size (queens-state-board-size queen-state))
		   (result-matrix (make-array (list size size))))
		(dolist (pos positions)
			(setf (aref result-matrix (position-x pos) (position-y pos)) t))
		result-matrix))

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


(defun gen-rotated-positions (pos size)
	(labels ((rotate-position-left! (pos board-size)
	(let ((lin (position-x pos))
		  (col (position-y pos)))
		(setf pos (create-position (- (- board-size 1) col) lin)))))
	(let ((result (list pos))
		  (copy-pos (create-position (position-x pos) (position-y pos))))
		(dotimes (n 3)
			(setf copy-pos (rotate-position-left! copy-pos size))
			(setf result (append result (list copy-pos))))
		result)))

;(defun rotated-positions?(pos1 pos2 size)
;	(let* ((rotated nil))
	;	(if (equal pos1 pos2)
	;		t
	;		(dotimes (n 3)
	;			(setf rotated (rotate-position-left pos2 size))
	;			(when (equal pos1 rotated)
	;				(return-from rotated-positions? t)))))
	;nil)




(defun operator (state)
	(let* ((size (queens-state-board-size state))
		   (sucessors (list))
		   (rotated-positions (list)))

		;(print '@@@@-FROM-THIS-STATE)
		;(print state)

		; only generate half of the lines*col combinations because the results are simmetrical
		(dotimes (l size)
			(when (free-line? state l)
				  (dotimes (c size)
					;(print 'here2)
					;(print state)
					;(format t "lin ~D  col ~D --- v: ~D ~%" l c (< c (- size l)))
					(when (and (free-column? state c) (free-diagonal? state l c) (null (member (create-position l c) rotated-positions :test #'equal-positions)))
						  (progn 
						  		(setf rotated-positions (append rotated-positions (gen-rotated-positions (create-position l c) size))) 
						 	 	(setf sucessors (append sucessors (list (result-of-move state l c)))))))))
				;(print 'skipping-line)))				


		;(print 'generated)
		;(print sucessors)
		;(print (length sucessors))
		
		sucessors))

(defun free? (state line column)
	(and (free-line? state line) (free-column? state column) (free-diagonal? state line column)))




