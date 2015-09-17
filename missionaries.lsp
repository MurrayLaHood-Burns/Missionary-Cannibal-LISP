#|
	Author: John Mangold
	Description: Checks the given state to see if it matches the goal state.
	Arguments: state
	Returns: t if states match, nil if states do not match
|#
(defun goal-state? (state)
	(when (equal state nil) (return-from goal-state? nil))
	(and (= (car state) 0) (= (cadr state) 0) (string= (caddr state) "right"))
)

; Generate-successors returns a list of successors to the current state.
#|
	Author: Murray Lahood-Burns
	Description: Given state it generates every possible successor state.
	Arguments: state
	Returns: succs - list containing the next possible states
		     
|#
(defun generate-successors (state)
	; define local variables
	(let ( (ml (car state)) (cl (cadr state)) (bank (caddr state)) mr cr
		   (succs nil) )
	
		; set number of missionaries and cannibals on right
		(setq mr (- *M* ml) cr (- *C* cl))
	
		; moves if canoe is on left bank
		(when (string= bank "left")
			; 1 missionary right
			(when (and (>= ml 1)
					   (or (>= (- ml 1) cl)
						   (= ml 1))
					   (>= (+ mr 1) cr))
				(setq succs (cons (list (- ml 1) cl "right") succs)))
				
			; 2 missionaries right
			(when (and (>= ml 2)
					   (or (>= (- ml 2) cl)
						   (= ml 2))
					   (>= (+ mr 2) cr))
				(setq succs (cons (list (- ml 2) cl "right") succs)))
				
			; 1 cannibal right
			(when (and (>= cl 1)
					   (or (>= mr (+ cr 1))
						   (= mr 0)))
				(setq succs (cons (list ml (- cl 1) "right") succs)))
				
			; 2 cannibals right
			(when (and (>= cl 2)
					   (or (>= mr (+ cr 2))
						   (= mr 0)))
				(setq succs (cons (list ml (- cl 2) "right") succs)))
				
			; 1 missionary and 1 cannibal right
			(when (and (>= cl 1)
					   (>= ml 1)
					   (>= (+ mr 1)
					   (+ cr 1)))
				(setq succs (cons (list (- ml 1) (- cl 1) "right") succs)))
		)
		
		; moves if canoes is on right bank
		(when (string= bank "right")
			; 1 missionary left
			(when (and (>= mr 1)
					   (or (>= (- mr 1) cr)
						   (= mr 1))
					   (>= (+ ml 1) cl))
				(setq succs (cons (list (+ ml 1) cl "left") succs)))
				
			; 2 missionaries left
			(when (and (>= mr 2)
					   (or (>= (- mr 2) cr)
						   (= mr 2))
					   (>= (+ ml 2) cl))
				(setq succs (cons (list (+ ml 2) cl "left") succs)))
				
			; 1 cannibal left
			(when (and (>= cr 1)
					   (or (>= ml (+ cl 1))
						   (= ml 0)))
				(setq succs (cons (list ml (+ cl 1) "left") succs)))
				
			; 2 cannibal left
			(when (and (>= cr 2)
					   (or (>= ml (+ cl 2))
						   (= ml 0)))
				(setq succs (cons (list ml (+ cl 2) "left") succs)))
			
			; 1 missionary and 1 cannibal left
			(when (and (>= cr 1)
					   (>= mr 1)
					   (>= (+ ml 1) (+ cl 1)))
				(setq succs (cons (list (+ ml 1) (+ cl 1) "left") succs)))
		)
		
		; return list of successors
		(return-from generate-successors succs)
	)
)

;-------------------------------------------------------------------------------

; print usage statement automatically upon loading file
(initial_usage)
