(defpackage kit-string/test
  (:use #:cl #:kit)
  (:export test))

(in-package #:kit-string/test)

(defun test ()
  (print (string-join (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "i22222222")))
  (time (loop repeat 2000000 do
              (loop for i in '("1" "2" "3" "4" "5" "6") do
                    (string-join (list i "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "i22222222"))))))

