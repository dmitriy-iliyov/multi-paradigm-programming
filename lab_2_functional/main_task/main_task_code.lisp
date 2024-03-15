(defun values-by-random (size &optional (result '()))
  (if (zerop size)
      result
      (values-by-random (1- size) (cons (random 100) result))))

(defun sort-list (array)
  (sort (subseq array 0 (length array)) '<))

(defun set-alphabet (alphabet-power &optional (index 0) (alphabet '()))
  (if (< index alphabet-power)
      (set-alphabet alphabet-power (1+ index) (cons (string (code-char (+ (char-code #\A) index))) alphabet))
      (reverse alphabet)))

(defun cut-to-intervals (sorted-array alphabet-power &optional (i 0) (matrix-interval '()))
  (if (= i alphabet-power)
      (reverse matrix-interval)
      (let* ((interval-width (/ (- (elt sorted-array (1- (length sorted-array))) (elt sorted-array 0)) (- alphabet-power 1)))
             (a (+ (elt sorted-array 0) (* i interval-width)))
             (b (+ (elt sorted-array 0) (* (1+ i) interval-width)))
             (new-interval (list a b)))
        (cut-to-intervals sorted-array alphabet-power (1+ i) (cons new-interval matrix-interval)))))

(defun to-char-list (start-list intervals alphabet)
  (let ((char-list (make-array (length start-list))))
    (dotimes (i (length start-list))
      (dotimes (j (length alphabet))
        (let ((a (elt (elt intervals j) 0))
              (b (elt (elt intervals j) 1)))
          (when (and (<= a (elt start-list i) b))
            (setf (elt char-list i) (elt alphabet j))))))
    char-list))

(defun find-index (a char-list &optional (index 0))
  (if (null char-list)
      nil
      (if (and (characterp a) (stringp (car char-list)) (string= a (car char-list)))
          index
          (find-index a (cdr char-list) (1+ index)))))

(defun make-result-matrix (char-array alphabet)
  (let* ((alphabet-power (length alphabet))
         (result-matrix (make-array (list alphabet-power alphabet-power)
                                    :initial-element 0)))
    (dotimes (i (length char-array))
      (let* ((current-char (aref char-array i))
             (current-index (if current-char
                                (find-index current-char alphabet)
                                nil)))
        (when current-index
          (let ((next-char (if (< (1+ i) (length char-array))
                                (aref char-array (1+ i))
                                nil))
                (next-index (if next-char
                                (find-index next-char alphabet)
                                nil)))
            (when next-index
              (incf (aref result-matrix current-index next-index)))))))
    result-matrix))

(defun print-matrix (matrix)
  (dotimes (i (array-dimension matrix 0))
    (dotimes (j (array-dimension matrix 1))
      (format t "~4d " (aref matrix i j)))
    (format t "~%")))

(defun main (size alphabet-power)
  (let ((start-list (values-by-random size))
        (alphabet (set-alphabet alphabet-power)))
    (let ((sorted-list (sort-list start-list)))
      (print "start list")
      (print start-list)
      (print "sorted list")
      (print sorted-list)
      (print "alphabet")
      (print alphabet)
      (format t "~%")
      (let ((intervals (cut-to-intervals sorted-list alphabet-power)))
        (dolist (interval intervals)
          (format t "Interval: [~f, ~f]~%" (first interval) (second interval)))
        (let ((char-list (to-char-list start-list intervals alphabet)))
          (format t "~A~%" char-list)
          (let ((result-matrix (make-result-matrix char-list alphabet)))
            (print-matrix result-matrix)))))))
        


(main 10 4)