(defpackage kit-top/test
  (:use #:cl #:kit)
  (:export test))

(in-package #:kit-top/test)

(defun test ()
  (printf #[1 2 3 4])
  (printf #{(1 2) ("a" #{(4 5)})}))

