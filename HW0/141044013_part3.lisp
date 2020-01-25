;;;; Inserts given number at the index to the sourceList
(defun insert-n (sourceList number index)
	(if (< index (length sourceList))
		(if (eql index 0)
			(cons number sourceList)
			(cons (pop sourceList) (insert-n sourceList number (- index 1)))
		)
		sourceList
	)
)