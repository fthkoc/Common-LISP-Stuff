;;;; Merges lists from target to source
(defun merge-list (sourceList targetList)
	(if (eql nil targetList)
		sourceList
		(cons (car targetList) (merge-list sourceList (cdr targetList)))
	)
)

