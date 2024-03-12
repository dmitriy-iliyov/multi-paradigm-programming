(handler-bind ((warning #'muffle-warning))
  (load "test.lisp")
  (format t "~%"))