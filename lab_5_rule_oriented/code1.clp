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

(defrule read-values
   =>
   (open "/Users/Sayner/github_repos/multi-paradigm-programming/unit_work/mpp_get_data_script/files/f_data.txt" data "r")
   (assert (values))
   (bind ?data (readline data))
   (while (neq ?data EOF)
       (assert (value (number (float ?data))))
       (bind ?data (readline data)))
   (close data)
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

(defrule cut-to-intervals
   ?sorted-array <- (sorted-array ?array)
   =>
   (bind ?size (length$ ?array))
   (bind ?interval-size (/ ?size ALPHABET_POWER))
   (bind ?start-index 0)
   (bind ?end-index (- ?interval-size 1))
   (loop-for-count (?i 0 ALPHABET_POWER)
      (bind ?start (nth$ ?start-index ?array))
      (bind ?end (nth$ ?end-index ?array))
      (assert (interval (start ?start) (end ?end)))
      (bind ?start-index (+ ?end-index 1))
      (bind ?end-index (+ ?end-index ?interval-size))
   )
)

(defrule print-intervals
   ?intervals <- (interval $?)
   =>
   (printout t "Intervals:" crlf)
   (foreach ?interval ?intervals
      (printout t "   " (fact-slot-value ?interval start) " - " (fact-slot-value ?interval end) crlf)
   )
   (printout t crlf)
)

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