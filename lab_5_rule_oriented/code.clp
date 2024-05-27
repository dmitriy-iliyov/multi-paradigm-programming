(deftemplate value
   (slot number (type FLOAT)))

(deftemplate interval
   (slot start (type FLOAT))
   (slot end (type FLOAT)))

(deftemplate alphabet
   (slot letter (type SYMBOL)))

(deftemplate transition
   (slot from (type SYMBOL))
   (slot to (type SYMBOL))
   (slot count (type INTEGER)))

(defrule generate-values
   =>
   (bind ?size 100)
   (assert (values))
   (loop-for-count (?i 0 ?size)
      (bind ?random-int (random 201))
      (bind ?random-float (/ ?random-int 200.0))
      (assert (value (number ?random-float)))
   )
)

(defrule print-initial-array
   ?values <- (values)
   ?value <- (value (number ?number))
   =>
   (printout t "Initial Array:" crlf)
   (printout t "   " ?number crlf)
   (printout t crlf)
)

(defrule sort-array
   ?values <- (values)
   =>
   (bind ?array (find-all-facts ((?f value)) TRUE))
   (bind ?sorted-array (sort ?array))
   (assert (sorted-array ?sorted-array))
)

(defrule print-sorted-array
   ?sorted-array <- (sorted-array $?array)
   =>
   (printout t "Sorted Array:" crlf)
   (printout t "   " (implode$ ?array) crlf)
   (printout t crlf)
)

(deffunction set-alphabet (?size)
   (bind ?start-char 65) ; ASCII code for 'A'
   (bind ?end-char (+ ?start-char (- ?size 1)))
   (loop-for-decimal (?ascii ?start-char ?end-char)
      (assert (alphabet (letter (char ?ascii))))
   )
)

(defrule set-alphabet
   =>
   (set-alphabet 26) ;
)

(defrule print-alphabet
   ?alphabets <- (alphabet $?letters)
   =>
   (printout t "Alphabet:" crlf)
   (printout t "   " (implode$ ?letters) crlf)
   (printout t crlf)
)

(deffunction reley-distribution (?x ?sigma)
  (if (< ?x 0) then
    0
    else
    (- 1 (exp (* -0.5 (pow (/ ?x ?sigma) 2))))))

(deffunction inverse-reley-distribution (?P ?sigma)
  (if (or (< ?P 0) (> ?P 1)) then
    -1
    else
    (if (eq ?P 1) then
      -1
      else
      (* ?sigma (sqrt (* -2 (log (- 1 ?P))))))))

(deffunction cut-to-intervals ()
  (bind ?summ 0)
  (loop-for-count (?i ?size)
    (bind ?summ (+ ?summ (* (nth$ ?i ?sorted_array) (nth$ ?i ?sorted_array)))))
  (bind ?sigma (sqrt (/ ?summ (* 2 ?size))))
  (printout t "sigma: " ?sigma crlf crlf)
  (bind ?interval (create$ 0 0))
  (bind ?a (nth$ 1 ?sorted_array))
  (loop-for-count (?i ?ALPHABET_POWER)
    (bind ?interval (create$ ?a))
    (bind ?pA (reley-distribution (nth$ 1 ?interval) ?sigma))
    (bind ?pB (+ (/ 1.0 ?ALPHABET_POWER) ?pA))
    (bind ?b (inverse-reley-distribution ?pB ?sigma))
    (bind ?interval (create$ ?a ?b))
    (bind ?a (nth$ 2 ?interval))
    (bind ?matrix_interval (create$ (nth$ 1 ?interval) (nth$ 2 ?interval)))
    (bind ?matrix_intervals (replace$ ?matrix_intervals ?i ?matrix_interval)))
  (bind ?i (- ?ALPHABET_POWER 1))
  (bind ?last_interval (nth$ ?i ?matrix_intervals))
  (if (> (nth$ 1 ?last_interval) (nth$ 2 ?last_interval)) then
    (bind ?interval_width (/ (- (nth$ 2 ?last_interval) (nth$ 1 (nth$ (- ?i 1) ?matrix_intervals))) 2))
    (bind ?last_interval (replace$ ?last_interval 2 (+ (nth$ 1 ?last_interval) ?interval_width)))
    (bind ?last_interval (replace$ ?last_interval 1 (nth$ 2 (nth$ (- ?i 1) ?matrix_intervals))))
    (bind ?matrix_intervals (replace$ ?matrix_intervals ?i ?last_interval)))
  (printout t "intervals:" crlf)
  (loop-for-count (?i ?ALPHABET_POWER)
    (printout t (str-cat (nth$ ?i ?alphabet) ": ["
                          (nth$ 1 (nth$ ?i ?matrix_intervals)) ", "
                          (nth$ 2 (nth$ ?i ?matrix_intervals)) "]\n")))
  (printout t crlf))


(defrule to-char-array
   ?intervals <- (interval $?)
   ?sorted-array <- (sorted-array ?array)
   =>
   (bind ?char-array "")
   (foreach ?number ?array
      (loop-for-count (?i 0 (length$ ?intervals))
         (if (<= ?number (fact-slot-value (nth$ ?i ?intervals) end))
            then
            (bind ?char-array (str-cat ?char-array (symbol-to-string (deftemplate-slot-value (fact-name (nth$ ?i ?intervals)) start))))
            break
         )
      )
   )
   (assert (char-array ?char-array))
)

(defrule print-char-array
   ?char-array <- (char-array ?array)
   =>
   (printout t "Character Array:" crlf)
   (printout t "   " ?array crlf)
   (printout t crlf)
)

(defrule make-result-matrix
   ?char-array <- (char-array ?array)
   =>
   (bind ?size (length$ ?char-array))
   (bind ?result-matrix (create$ (do-for-all-facts ((?f alphabet)) (fact-slot-value ?f letter))))
   (loop-for-count (?i 0 ?size)
      (bind ?from (nth$ ?i ?char-array))
      (bind ?to (nth$ (+ ?i 1) ?char-array))
      (if (neq ?to "")
         then
         (bind ?from-index (find-index-in-fact-slot-value ?from ?result-matrix))
         (bind ?to-index (find-index-in-fact-slot-value ?to ?result-matrix))
         (bind ?count (fact-slot-value (nth$ ?from-index ?result-matrix) ?to))
         (modify-item ?result-matrix ?from-index (create$ ?to (+ ?count 1)))
      )
   )
   (assert (result-matrix ?result-matrix))
)

(defrule print-result-matrix
   ?result-matrix <- (result-matrix $?matrix)
   =>
   (printout t "Result Matrix:" crlf)
   (printout t "   " (implode$ (do-for-all-facts ((?f alphabet)) (fact-slot-value ?f letter))) crlf)
   (foreach ?row ?matrix
      (printout t (nth$ 0 ?row) " " (implode$ (rest$ ?row)) crlf)
   )
   (printout t crlf)
)