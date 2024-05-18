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

(defun reley-distribution (x sigma)
  (if (< x 0)
      0
      (- 1 (exp (*  -0.5 (expt (/ x sigma) 2))))))

(defun inverse-reley-distribution (P sigma)
  (if (or (< P 0) (> P 1))
      -1
      (if (= P 1)
          -1
          (* sigma (sqrt (* -2 (log (- 1 P))))))))

(defun cut-to-intervals (sorted-array alphabet-power)
  "cut list to intervals based on rayleigh distribution"
  (let* ((size (length sorted-array))
         (summ (reduce #'+ (mapcar (lambda (x) (* x x)) sorted-array)))
         (sigma (sqrt (/ summ (* 2 size))))
         (matrix-interval '())
         (buffer (elt sorted-array 0)))
    (format t "Sigma: ~a~%" sigma)
    (dotimes (i alphabet-power)
      (let* ((pA (reley-distribution buffer sigma))
             (b (inverse-reley-distribution (+ (/ 1.0 alphabet-power) pA) sigma)))
        (format t "Buffer: ~a, pA: ~a, b: ~a~%" buffer pA b)
        (push (list buffer b) matrix-interval)
        (setf buffer b)))
    (setf (caadr matrix-interval) (elt sorted-array (1- size)))
    (reverse matrix-interval)))


(defun to-char-list (start-list intervals alphabet)
  "convert list of numbers into a list of characters based on intervals"
  (let ((char-list (make-list (length start-list))))
    (dotimes (i (length start-list))
      (let ((current-number (nth i start-list)))
        (dotimes (j (length alphabet))
          (let ((a (nth 0 (nth j intervals)))
                (b (nth 1 (nth j intervals)))
                (current-char (nth j alphabet)))
            (when (and (<= a current-number b))
              (setf (nth i char-list) current-char))))))
    char-list))

(defun find-index (element lst &optional (index 0))
  (cond ((null lst) nil)
        ((eql element (car lst)) index)
        (t (find-index element (cdr lst) (+ index 1)))))

(defun make-result-matrix (char-list alphabet)
  (let* ((alphabet-power (length alphabet))
         (result-matrix (make-array (list alphabet-power alphabet-power) :initial-element 0)))
    (loop for i below (1- (length char-list))
          for current-char = (nth i char-list)
          for next-char = (if (< (1+ i) (length char-list)) (nth (1+ i) char-list) nil)
          do (let* ((current-index (find-index current-char alphabet))
                    (next-index (find-index next-char alphabet)))
               (when (and current-index next-index)
                 (incf (aref result-matrix next-index current-index)))))
    result-matrix))

(defun print-matrix (matrix)
  (loop for i below (array-dimension matrix 0)
        do (loop for j below (array-dimension matrix 1)
                 do (format t "~a " (aref matrix i j)))
           (format t "~%")))

(defun main (size alphabet-power)
  (let ((start-list (values-by-random size))
        (alphabet (set-alphabet alphabet-power)))
    (let ((sorted-list (sort-list start-list)))
      (format t "default list: ~a~%sorted list: ~a~%alphabet: ~a~%" start-list sorted-list alphabet)
      (let ((intervals (cut-to-intervals sorted-list alphabet-power)))
        (dolist (interval intervals)
          (format t "interval: [~f, ~f]~%" (first interval) (second interval)))
        (let ((char-list (to-char-list start-list intervals alphabet)))
          (format t "char list: ~A~%" char-list)
          (let ((result-matrix (make-result-matrix char-list alphabet)))
            (print-matrix result-matrix)))))))

(main 4 4)
