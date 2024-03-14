(handler-bind ((warning #'muffle-warning))
  (load "main_task_code.lisp")
  (format t "~%"))