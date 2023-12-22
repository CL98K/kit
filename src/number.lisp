(in-package #:kit)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(declaim (ftype (function (simple-array) double-float) parse-float) (inline parse-float))
(defun parse-float (string)
  "从字符串中解析浮点数"
  (multiple-value-bind (integer i) (parse-integer string :junk-allowed t)
    (declare (type fixnum integer) (type (mod 1024) i))
    
    (if (and (> (array-total-size string) i) (char= (schar string i) #\.))
        (multiple-value-bind (fraction j) (parse-integer string :start (+ i 1) :junk-allowed t)
          (declare (type (or fixnum null) fraction) (type fixnum j))
          (if fraction
              (+ integer (/ fraction (expt 10.0d0 (- j i 1))))
              (+ integer 0.0d0)))
        (+ integer 0.0d0))))

(declaim (ftype (function (simple-array) boolean) digit-string-p) (inline digit-char-p))
(defun digit-string-p (string)
  "判断是否是数字字符串"
  (let ((size (array-total-size string)))
    (declare (type (mod 1024) size))
    (dotimes (i size)
      (if (not (digit-char-p (schar string i)))
          (return-from digit-string-p nil))))
  t)

(declaim (ftype (function (integer) simple-bit-vector) integer-to-bits) (inline integer-to-bits))
(defun integer-to-bits (integer)
  "数字到二进制"
  (let* ((size (integer-length integer))
         (bits (make-array size :element-type 'bit :initial-element 0)))
    (declare (type fixnum size) (type simple-array bits))

    (dotimes (index size bits)
      (if (logbitp index integer) (setf (aref bits (1- (- size index))) 1)))))

(declaim (ftype (function (simple-bit-vector) integer) bits-to-integer) (inline bits-to-integer))
(defun bits-to-integer (bit-vector)
  "二进制到数字"
  (reduce #'(lambda (first-bit second-bit) (+ (* first-bit 2) second-bit)) bit-vector))

(declaim (ftype (function (fixnum fixnum &key (:order symbol) (:signed boolean)) simple-array) integer-to-bytes) (inline integer-to-bytes))
(defun integer-to-bytes (integer length &key (order :big) (signed nil))
  "数字转字节数组"
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type fixnum integer length) (type symbol order) (type boolean signed))
  
  (if (and (not (eq order :big)) (not (eq order :little))) (error "The 'order' can only be: big or :little"))
  
  (let* ((temp 0)
         (flag (if (eq order :big) t nil))
         (index (if flag 0 (1- length)))
         (signed (if (and (< integer 0) signed) t nil))
         (element (if signed 0 1))
         (bits (make-array (the fixnum (* length 8)) :element-type 'bit :initial-element (if signed 1 0)))
         (bits-length (1- (array-total-size bits)))
         (bytes (make-array length :element-type '(unsigned-byte 8) :initial-element 0))
         (table (make-array 8 :element-type '(mod 129) :initial-contents '(128 64 32 16 8 4 2 1))))
    (declare (type (mod 256) temp) (type boolean flag signed) (type bit element) (type fixnum index bits-length) (type simple-array bits bytes table))
    
    (loop for i fixnum from bits-length downto 0
          for v fixnum from 0 to (integer-length integer)
          do
          (if signed
              (if (not (logbitp v integer)) (setf (aref bits i) element))
              (if (logbitp v integer) (setf (aref bits i) element))))

    (loop for i fixnum from 0 to bits-length
          for j fixnum = (rem i 8)
          do
          (if (and (= j 0) (/= i 0))
              (progn
                (setf (aref bytes index) temp)
                (setf temp 0)
                (if flag (incf index) (decf index))))
          (incf temp (* (aref bits i) (aref table j))))
    
    (setf (aref bytes index) temp)

    (return-from integer-to-bytes bytes)))

(declaim (ftype (function (simple-array &key (:order symbol) (:signed boolean)) integer) bytes-to-integer) (inline bytes-to-integer))
(defun bytes-to-integer (bytes &key (order :big) (signed nil))
  "字节数组转数字"
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
           (type (simple-array (unsigned-byte 8) *) bytes) (type symbol order) (type boolean signed))
  
  (if (and (not (eq order :big)) (not (eq order :little))) (error "The 'order' can only be: big or :little"))
  
  (let* ((integer 0)
         (flag (if (eq order :big) t nil))
         (element (if signed 0 1))
         (bytes-size (array-total-size bytes))
         (bits-length (* bytes-size 8))
         (bits (make-array bits-length :element-type 'bit :initial-element (if signed 1 0))))
    (declare (type bit element) (type fixnum integer bytes-size bits-length) (type simple-array bits) (type boolean flag))
    
    (loop for i fixnum from 0 by 8
          for j fixnum from 0 below bytes-size
          for v fixnum = (if flag (aref bytes j) (aref bytes (- bytes-size (1+ j))))
          do
          (loop for x fixnum downfrom 7 to 0
                for y fixnum from i
                do
                (if signed              
                    (if (not (logbitp x v)) (setf (aref bits y) element))
                    (if (logbitp x v) (setf (aref bits y) element)))))
    
    (loop for i fixnum downfrom (1- bits-length) to 0
          for j fixnum from 0
          for v bit = (aref bits i)
          for flag = (if (= v 0) nil t)
          do
          (incf integer (* (if signed (if flag 1 v) v) (the fixnum (expt 2 j)))))

    (return-from bytes-to-integer (if (and signed (= (aref bits 0) 1)) (- integer (the fixnum (expt 2 bits-length))) integer))))

