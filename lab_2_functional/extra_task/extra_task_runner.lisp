(handler-bind ((warning #'muffle-warning))
  (load "extra_task_code.lisp")
  (format t "~%"))