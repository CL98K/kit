(defpackage kit-number/test
  (:use #:cl #:kit)
  (:export test))

(in-package #:kit-number/test)

(defun test ()
  (format t "~A~%" (parse-float "123"))
  (format t "~A~%" (parse-float "123.0"))
  (format t "~A~%" (parse-float "0"))
  (format t "~A~%" (parse-float "-1"))
  (format t "~A~%" (parse-float "1."))

  (format t "~A~%" (digit-string-p "1"))
  (format t "~A~%" (digit-string-p "1."))
  (format t "~A~%" (digit-string-p "abc"))
  (format t "~A~%" (digit-string-p "-1"))

  (format t "~A~%" (bytes-to-integer (integer-to-bytes 12312312112312312 16 :order :big :signed t) :order :big :signed t))
  (format t "~A~%" (bytes-to-integer (integer-to-bytes -12312312112312312 16 :order :big :signed t) :order :big :signed t))
  )
