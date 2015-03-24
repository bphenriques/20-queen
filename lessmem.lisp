;Bruno Alexandre Pires Henriques
;72913

;(time (resolve-problema (make-array '(6 6)) "a*"))
;Real time: 3.192726 sec.
;Run time: 3.185578 sec.
;Space: 115946888 Bytes
;GC: 115, GC time: 0.631295 sec.
;#2A((NIL T NIL NIL NIL NIL)
;    (NIL NIL NIL T NIL NIL)
;    (NIL NIL NIL NIL NIL T)
;    (T NIL NIL NIL NIL NIL)
;    (NIL NIL T NIL NIL NIL)
;    (NIL NIL NIL NIL T NIL))


;============================================
;================ QUEEN STATE ===============
;============================================


(defun 2d-array-to-list (array)
  (map 'list (lambda (x) x) array))

(defun put-queen! (state lin col)
		(setf (svref state lin) col))

(defun result-of-move (state lin pos)
	(let ((state-copy (copy-array state)))
			(put-queen! state-copy lin pos)
			state-copy))

(defun free-line? (state line)
	;(format t "free-line? ~D~%" line) 
	(null (svref state line)))

(defun free-column? (lst-pos column)
	(null (member column lst-pos :key #'position-y)))

; PROBLEMA HERE
(defun gen-list-positions(state)
	(let ((result (list)))
		(dotimes (l (length state))
			(let ((col (svref state l)))
				(when (not (null col))
					(setf result (append result (list (list l col)))))))
		result))

(defun free-diagonal? (lst-pos line column)
	;(format t "free-diagonal? ~D ~D~%" line column)
		(dolist (pos lst-pos)
			(let ((l (first pos))
				  (c (second pos)))
				;(format t "|dif lines| = ~D  == |dif cols| = ~D" (abs (- l line)) (abs (- c column)))
				(when (= (abs (- l line))
			 	 	     (abs (- c column)))
			 	 	  (return-from free-diagonal? nil))))
			 	 	;(format t "trying next position"))))
		;(print result)
		t)
			 	 	;(format t "trying next position"))))
		;(print result)

(defun convert-board-to-queens-state(matrix)
	(let* ((size (array-dimension matrix 0))
		   (state (make-array size)))
		(dotimes (line size)
			(dotimes (column size)
				(when (eq (aref matrix line column) t)
					(put-queen! state line column)
					(return))))
		state))

(defun convert-queens-state-to-board(state)
	(let* ((size (array-dimension state 0))
		   (result-matrix (make-array (list size size))))
		(dotimes (l size)
			(let ((col (svref state l)))
				(setf (aref result-matrix l col) t)))
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
		(print result-state)
		(setf result-state (first (last (nth (- (length result-state) 4) result-state))))

		(when (not (null result-state))
			  (setf transformed-result (convert-queens-state-to-board result-state)))

		transformed-result))
		



; WAAAAS HEEEREEEE

(defun count-queens (state)
	(length (filtra #'(lambda (x) (not (null x))) (2d-array-to-list state))))

(defun objective? (state)
	(= (length state) (count-queens state)))


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

; USEFUL snippets
;> (member-if #>oddp '( 2 3 4))
; (3 4) 

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


(defun operator (state)
	(let* ((size (length state))
		   (sucessors (list))
		   (rotated-positions (list))
		   (lst-pos (gen-list-positions state)))

		;(print '@@@@-FROM-THIS-STATE)
		;(print state)

		; only generate half of the lines*col combinations because the results are simmetrical
		(dotimes (l size)
			(when (free-line? state l)
				  (dotimes (c size)
					;(print 'here2)
					;(print state)
					;(format t "lin ~D  col ~D --- v: ~D ~%" l c (< c (- size l)))
					(when (and (free-column? lst-pos c) (free-diagonal? lst-pos l c) (null (member (create-position l c) rotated-positions :test #'equal-positions)))
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




