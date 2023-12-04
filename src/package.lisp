(eval-when (:compile-toplevel :load-toplevel)
  (defmacro defdelim (dispatch left right parms &body body)
    `(ddfn ,dispatch ,left ,right #'(lambda ,parms ,@body)))
  
  (let ((rpar (get-macro-character #\))))
    (defun ddfn (dispatch left right fn)
      (set-macro-character right rpar)
      (set-dispatch-macro-character dispatch left
                                    #'(lambda (stream char1 char2)
                                        (declare (ignore char1 char2))
                                        (funcall fn (read-delimited-list right stream t))))))

  (defdelim #\# #\[ #\] (args)
    "快速 list 表达式"
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
             (type cons args))
    (list 'quote (loop for val in args collect val)))

  (defdelim #\# #\{ #\} (args)
    "快速 dict 表达式"
    (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
             (type cons args))
    (let ((hash-table (make-hash-table :test 'equal)))
      (when args
        (loop for (key val) in args do (setf (gethash key hash-table) val)))
      hash-table)))

(cl:defpackage #:kit
  (:documentation "first aid kit")
  (:use #:cl)
  (:export
   #:*line-width*
   #:printf
   #:print-list-readably
   #:print-hash-table-readably
   #:parse-float
   #:digit-string-p
   #:integer-to-bits
   #:bits-to-integer
   #:integer-to-bytes
   #:bytes-to-integer
   #:parse-ts
   #:parse-xml
   #:parse-csv
   #:string-join))
