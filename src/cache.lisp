(in-package #:kit)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)  
  (defmacro lru-cache ((name args docment declaration body) &key (max-size 32) (timeout nil) &allow-other-keys)
    (let* ((hash-keys (loop with buffer
                            for i in args
                            do
                            (typecase i
                              (symbol (if (char/= (char (string i) 0) #\&) (push i buffer)))
                              (cons (push (car i) buffer)))
                            finally (return (nreverse buffer))))
           (hash-op (loop for key in hash-keys collect `(typecase ,key
                                                          (null 0)
                                                          (boolean 1)
                                                          ((or fixnum integer bignum) ,key)
                                                          (t (sxhash ,key)))))
           (key (gensym))
           (cache (gensym)))
      
      `(let ((,key nil)
             (,cache (make-hash-table :test 'eq :size ,max-size)))
         (defun ,name ,args
           ,docment
           ,declaration
           (setf ,key (+ ,@hash-op))
           
           (multiple-value-bind (val flag) (gethash ,key ,cache)
             (declare (type boolean flag))
             (if flag (return-from ,name val)))
           
           (setf (gethash ,key ,cache) (progn ,@body))
           (return-from ,name (gethash ,key ,cache))))))

  (set-macro-character #\@ (lambda (stream char)
                             (declare (ignore char))
                             (let ((decorator (read stream nil))
                                   (params (read stream nil))
                                   (form nil)
                                   (func-name nil)
                                   (func-args nil)
                                   (func-docment nil)
                                   (func-declare nil)
                                   (func-body nil))
                               (if (eq (car params) 'defun)
                                   (progn
                                     (setf form params)
                                     (setf params nil))
                                   (setf form (read stream nil)))

                               (setf func-name (cadr form))
                               (setf func-args (caddr form))
                               (setf func-docment (if (stringp (cadddr form)) (cadddr form)))
                               (if func-docment (setf form (cddddr form)))
                               (setf func-declare (if (and (listp (car form)) (eq (caar form) 'declare)) (car form)))
                               (if func-declare (setf form (cdr form)))

                               (if (and (not func-docment) (not func-declare))
                                   (setf func-body (cdddr form))
                                   (setf func-body form))

                               `(,decorator (,func-name ,func-args ,func-docment ,func-declare ,func-body) ,@params)))))


;; @lru-cache (:max-size 100 :timeout 12 :x 1231)
;; (defun test (arg arg2 &key x)
;;   "asdas"
;;   (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3))
;;            (type string arg) (type fixnum arg2))
