(defun values-by-random (length)
  (loop repeat length collect (random 100)))

(sort (values-by-random 7) '<)

