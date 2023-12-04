(in-package #:kit)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(defvar *line-width* 80)

(defun output (string &optional (stream *standard-output*))
  (write-string string stream)
  (length string))

(defun print-list-readably (list &optional (stream *standard-output*) (indent 0))
  (declare (type list list) (type stream stream) (type fixnum indent))
  (let ((*print-circle* t)
        (*print-readably* nil)
        (init-indent indent)
        (count 0))
    
    (incf indent (output (format nil " (") stream))
    
    (dolist (elm list)
      (typecase elm
        (hash-table (progn
                      (print-hash-table-readably elm stream indent)
                      (output (format nil "~%~A" (make-string indent :initial-element #\ )) stream)))
        (t (progn
             (output (format nil "~S " elm) stream)
             (incf count)
             (when (>= count *line-width*)
               (output (format nil "~%~A" (make-string indent :initial-element #\ )) stream)
               (setf count 0))))))

    (output (format nil ")") stream)
    (- indent init-indent)))

(defun print-hash-table-readably (hash-table &optional (stream *standard-output*) (indent 0))
  (declare (type hash-table hash-table) (type stream stream) (type fixnum indent))
  (let ((*print-circle* t)
        (*print-readably* nil)
        (init-indent indent))

    (incf indent (output (format nil " {") stream))
    
    (with-hash-table-iterator (generator-fn hash-table)
      (loop
        (multiple-value-bind (more? key value) (generator-fn)
          (unless more? (return))                             
          (typecase value
            (hash-table (progn
                          (if (= (hash-table-count value) 0)
                              (output (format nil "~%~A~S: {}," (make-string indent :initial-element #\ ) key) stream)
                              (progn
                                (setf indent (1- (output (format nil "~%~A~S:" (make-string indent :initial-element #\ ) key))))
                                (setf indent (+ (print-hash-table-readably value stream indent) init-indent))
                                (output (format nil ", ") stream)))))
            (list (progn
                    (setf indent (1- (output (format nil "~%~A~S:" (make-string indent :initial-element #\ ) key) stream)))
                    (setf indent (+ (print-list-readably value stream indent) init-indent))
                    (output (format nil ", ") stream)))
            (t (progn
                 (output (format nil "~%~A~S: ~S," (make-string indent :initial-element #\ ) key value) stream)))))))
    
    (output (format nil "}") stream)
    (- indent init-indent)))

(defun printf (object &optional (stream *standard-output*) (indent 0))
  (typecase object
    (list (print-list-readably object stream indent))
    (hash-table (print-hash-table-readably object stream indent))
    (t (pprint object stream))))
