(in-package #:kit)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(declaim (ftype (function (sequence &key (:sep simple-string)) string) string-join) (inline string-join))
(defun string-join (sequences &key (sep ","))
  "字符串拼接"
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type list sequences) (type simple-string sep))
  (let* ((index 0)
         (seq-len (length sequences))
         (string-size (+ (loop for i in sequences sum (length i)) (1- (* seq-len (length sep)))))
         (buffer (make-string string-size)))
    (declare (type fixnum index seq-len string-size) (type string buffer))
    (dolist (string sequences buffer)
      (declare (type simple-string string))
      (loop for c of-type character across string do
            (setf (schar buffer index) c)
            (incf index))
      (when (< index string-size)
        (loop for c of-type character across sep do
              (setf (schar buffer index) c)
              (incf index))))))

