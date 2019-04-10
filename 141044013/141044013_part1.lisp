;;;; Takes a list, changes it to the one dimentional list if there is any lists as an element of the list.
(defun list-leveller (nestedList)
	(if (not (eql nil nestedList))
		(if (listp (car nestedList))
			(append (list-leveller (car nestedList)) (list-leveller(cdr nestedList)))
			(cons (car nestedList) (list-leveller(cdr nestedList)))
		)
	)
)