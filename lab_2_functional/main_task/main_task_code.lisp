(defun values-by-random (size)
  (loop repeat size collect (random 100)))

(defun sort-list (userList)
  (setq sortedList (sort (subseq userList 0 (length userList)) '<)))

(defun main (size alphabet-power)
  (setq startList (values-by-random size))
  (sort-list startList)
  (print sortedList))

(main 10 4)