;Bruno Alexandre Pires Henriques
;72913

;============================================
;================ QUEEN STATE ===============
;============================================

(defstruct queens-state number-placed positions ocupied-columns)

(defun empty-queens-state (size)
	(make-queens-state 	:number-placed 0
					 	:positions (make-array size)
					 	:ocupied-columns (make-array size)))

(defun make-copy-queens-state (state)
	(make-queens-state 	:number-placed (queens-state-number-placed state)
					 	:positions (copy-array (queens-state-positions state))
					 	:ocupied-columns (copy-array (queens-state-ocupied-columns state))))

(defun put-queen! (state lin col)
		(setf (svref (queens-state-positions state) lin) col)
		(setf (svref (queens-state-ocupied-columns state) col) t)
		(incf (queens-state-number-placed state)))

(defun result-of-move (state lin pos)
	(let ((state-copy (make-copy-queens-state state)))
			(put-queen! state-copy lin pos)
			state-copy))

(defun free-line? (state line)
	(null (svref (queens-state-positions state) line)))

(defun free-column? (state column)
	(null (svref (queens-state-ocupied-columns state) column)))

(defun free-diagonal? (state line column)
	(let ((positions (queens-state-positions state)))
		(dotimes (l (array-dimension positions 0))
			(let ((c (svref positions l)))
				(when (and (not (null c))
						   (= (abs (- l line))
			 	 	     	  (abs (- c column)))
			 	 	  (return-from free-diagonal? nil))))))
		t)

(defun free? (state l c)
	(and (free-line? state l)
		 (free-column? state c)
		 (free-diagonal? state l c)))

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
		   (size (array-dimension positions 0))
		   (result-matrix (make-array (list size size))))
		(dotimes (l size)
			(setf (aref result-matrix l (svref positions l)) t))
		result-matrix))


;=================================================
;==============   POSITIONS ======================
;=================================================

(defun equal-positions (pos1 pos2)
	(equal pos1 pos2))

(defun gen-rotated-positions (pos size)
	(labels ((rotate-position-left! (pos size)
		(let ((lin (car pos))
		  	  (col (cdr pos)))
				(setf pos (create-position (- (- size 1) col) lin)))))

	(let ((result (cons pos nil))
		  (copy-pos (create-position (car pos) (cdr pos))))
		(dotimes (n 3)
			(setf copy-pos (rotate-position-left! copy-pos size))
			(setf result (cons copy-pos result)))
		result)))


(defun create-position (lin col)
	(cons lin col))


; Name: resolve-problema
; Arguments: initial-state, strategy
; Return: Bi-dimensional array with the queens position; nil if there is no solution
; Note:
(defun resolve-problema (initial-state strategy)
	(let* ((initial-state-transformed (convert-board-to-queens-state initial-state))
		   (result-state nil)
		   (transformed-result nil))

		(setf result-state (procura (cria-problema initial-state-transformed (list #'operator) :objectivo? #'objective? :heuristica #'heuristic) strategy))
		
		;(print result-state)
		(setf result-state (first (last (nth (- (length result-state) 4) result-state))))

		(when (not (null result-state))
			  (setf transformed-result (convert-queens-state-to-board result-state)))

		transformed-result))

(defun operator (state)
	(let* ((size (array-dimension (queens-state-positions state) 0))
		   (sucessors (list))
		   (rotated-positions (list)))

		(dotimes (l size)
			(when (free-line? state l)
				  (dotimes (c size)
				  	(when (and (free-column? state c) 
							   (free-diagonal? state l c)
							   (null (member (create-position l c) rotated-positions :test #'equal-positions)))
						  (progn 
						  		(setf rotated-positions (append rotated-positions (gen-rotated-positions (create-position l c) size)))
						 	 	(setf sucessors (append sucessors (list (result-of-move state l c)))))))
				  (return-from operator sucessors)))))


(defun objective? (state)
	(= (queens-state-number-placed state) (array-dimension (queens-state-positions state) 0)))
		
;pri
;previlegia o preenchimento das primeiras linhas
(defun heuristic (state)
  (let* ((heuristic 0)
         (positions (queens-state-positions state))
         (size (array-dimension positions 0))
         (n-free 0))
    (dotimes (l size)
    	(let ((n-conflits 0))
	      	(when (null (svref positions l))
	        	  (incf n-free)
	          	(dotimes (c size)
	            	(when (not (free? state l c))
	                	  (incf n-conflits))))
	      	;give more weight to sum of conflicts nearest to first line (beggining of the processing)
      		(setf heuristic (+ heuristic (* n-free n-conflits)))))
    ;give more weight globally to the board when it is more free
    (* heuristic n-free)))