(defun values-by-random (size)
  (loop repeat size collect (random 100)))

(defun sort-list (userList)
  (setq sortedList (sort (subseq userList 0) '<)))

(defun main (size alphabetPower)
  (setq startList (values-by-random size))
  (sort-list startList)
  (print "start list")
  (print startList)
  (print "sorted list")
  (print sortedList))

(main 10 4)