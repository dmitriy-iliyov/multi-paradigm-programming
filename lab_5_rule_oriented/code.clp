(defrule print-lines
   =>
   (open "/Users/Sayner/github_repos/multi-paradigm-programming/lab_5_rule_oriented/data.txt" data "r")
   (bind ?data (readline data))
   (while (neq ?data EOF)
       (printout t ?data crlf)
       (bind ?data (readline data)))
   (close data)
)