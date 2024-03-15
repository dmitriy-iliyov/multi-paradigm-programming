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
      matrix-interval
      (let* ((interval-width (/ (- (elt sorted-array (1- (length sorted-array))) (elt sorted-array 0)) (- alphabet-power 1)))
             (a (+ (elt sorted-array 0) (* i interval-width)))
             (b (+ (elt sorted-array 0) (* (1+ i) interval-width)))
             (new-interval (list a b)))
        (cut-to-intervals sorted-array alphabet-power (1+ i) (cons new-interval matrix-interval)))))


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
          (format t "Interval: [~f, ~f]~%" (first interval) (second interval)))))))

(main 10 4)