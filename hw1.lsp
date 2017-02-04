;PAD takes an ingteger argument x and returns xth number of the sequence defined by the following recursive relation:
;PAD(x+1) = PAD(x-1) + PAD (x-2) with PAD(0) = PAD(1) = PAD(2) = 1
(defun PAD (x) 
     (cond ((< x 0) nil) ;if x<0, it is an invalid input, return nil.
	    ((< x 3) 1) ;if x = 0, 1 or 2, PAD returns 1
	     (t (+ (PAD(- x 2)) (PAD(- x 3)))) ;otherwise, PAD(x) = PAD(x-2) + PAD (x-3)
	      ))

;SUMS takes an integer argument x and returns number of additions PAD funtion takes to compute (PAD x)
(defun SUMS (x)
  (cond ((< x 0) nil) ;if x<0, it is an invalid input, return nil
	((< x 3) 0); if x = 0, 1 or 2, it takes no addition, SUMS returns 0
	(t (+ (+ (SUMS (- x 2)) (SUMS (- x 3)) 1))) 
	; otherwise, it returns the sum of the number of additions to calculate (PAD x-2) and that to calculate (PAD x-3) plus another addition to add (PAD x-2) and (PAD x-3) together.
	)
  )
;ANON takes a single argument TREE that represents a tree and returns an anonymized tree with the same structure,
;but where all symbols and numbers in the tree are replaced by a queston mark.
(defun ANON (TREE)
  (cond ((listp TREE) (cond ((null TREE) nil)
  ; TREE has more than one element, it is represented by a list; if ths list is empty, return nil
			    ((listp (car TREE)) (cons (ANON (car TREE)) (ANON (cdr TREE)) )) 
			    ;if the first element of TREE is a list, execute ANON on this list and the rest of the TREE
			    (t (cons '? (ANON (cdr TREE)) ))) ) 
			    ; if the element is an atom, replace this atom with a "?" and execute ANON on the rest of the TREE
	(t '?) ;if TREE only has one element, replace it with a "?".
	)
  )
