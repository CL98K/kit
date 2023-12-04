(in-package #:kit)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 3)))

(declaim (inline *init-time*))
(defparameter *init-time* (encode-universal-time 0 0 8 1 1 1970))

(declaim (inline parse-ts))
(defun parse-ts (timestamp)
  "解析时间戳生成对应的 天戳, 小时戳, 分钟戳, 小时, 分钟, 秒"
  (declare (type fixnum timestamp) (dynamic-extent timestamp))
  (multiple-value-bind
        (second minute hour)
      (decode-universal-time (+ *init-time* timestamp))

    (values
     (truncate (/ (+ timestamp 28800) 86400))
     (truncate (/ timestamp 3600))
     (truncate (/ timestamp 60))
     (1+ hour)
     (+ (* hour 60) minute)
     (+ (* (+ (* hour 60) minute) 60) second))))

(defun parse-xml (filename &key skip-root-p auto-parse-type-p (auto-parse-type-func 'read-from-string))
  "解析 XML 生成哈希结构"
  ;; skip-first-p 是否跳过根节点
  ;; auto-parse-type-p 是否自动解析属性类型
  ;; auto-parse-type-func 自动解析属性类型函数(单参数)
  
  (declare (type simple-array filename))
  (labels ((attr-proc (attr text)
             (let ((hash-table (make-hash-table :test 'equal))
                   (attr-hash-table (make-hash-table :test 'equal)))
               (if attr
                   (loop for (key val) of-type simple-string in attr
                         do
                         (setf (gethash key attr-hash-table)
                               (if auto-parse-type-p (funcall (symbol-function auto-parse-type-func) val) val))))
               (setf (gethash ":attr" hash-table) attr-hash-table)
               (setf (gethash ":text" hash-table) text)               
               hash-table))
           
           (parse-struct (tree buffer)
             (let* ((tag (car tree))
                    (text (car (last tree)))
                    (text (if (typep text 'simple-array) (string-trim '(#\Space #\Tab #\Newline) text) ""))
                    (attr-hash-table (attr-proc (cadr tree) text))
                    (childs (remove-if #'(lambda (x) (not (typep x 'cons))) (cddr tree)))
                    (child-num (list-length childs)))

               (declare (type simple-array tag text)
                        (type hash-table attr-hash-table)
                        (type cons childs)
                        (type (mod 1024) child-num))

               (typecase buffer
                 (hash-table (progn
                               (cond
                                 ((not (gethash tag buffer)) (setf (gethash tag buffer) attr-hash-table))
                                 ((typep (gethash tag buffer) 'hash-table)
                                  (setf (gethash tag buffer)
                                        (list (alexandria:copy-hash-table (gethash tag buffer))))))
                               (if (typep (gethash tag buffer) 'list)
                                   (alexandria:appendf (gethash tag buffer) (list attr-hash-table)))))
                 (t (alexandria:appendf buffer (list attr-hash-table))))

               (if (and (= child-num 0) (typep (gethash tag buffer) 'hash-table))
                   (setf (gethash tag buffer) (list (alexandria:copy-hash-table (gethash tag buffer)))))
               
               (dolist (child childs)
                 (if (typep (gethash tag buffer) 'hash-table)
                     (parse-struct child (gethash tag buffer))
                     (parse-struct child (car (last (gethash tag buffer)))))))))
    
    (let ((xmls-list-struct (cxml:parse-file filename (cxml-xmls:make-xmls-builder)))
          (xmls-dict-struct (make-hash-table :test 'equal)))
      
      (declare (type cons xmls-list-struct) (type hash-table xmls-dict-struct))
      (parse-struct xmls-list-struct xmls-dict-struct)

      (when skip-root-p
        (setf xmls-dict-struct (gethash (car xmls-list-struct) xmls-dict-struct))
        (remhash ":attr" xmls-dict-struct)
        (remhash ":text" xmls-dict-struct))
      
      xmls-dict-struct)))

(defun parse-csv (filename key-row value-row &key (external-format :utf-8) skip-first-p auto-parse-type-p (auto-parse-type-func 'read-from-string))
  "从 CSV 文件构建资源哈希结构"
  ;; key-row 可以是 number 或 list, 从 1 开始
  ;; value-row 可以是 number 或 list 或 plist, 从 1 开始
  ;; skip-first-p 是否跳过表头
  ;; auto-parse-type-p 是否自动解析属性类型
  ;; auto-parse-type-func 自动解析属性类型函数(单参数)
  
  ;; example:
  ;; (parse-vcsv "xx.csv" 1 1)
  ;; (parse-vcsv "xx.csv" 1 '(1 2 3))
  ;; (parse-vcsv "xx.csv" 1 '("a" 1 "b" 2 "c" 3))
  ;; (parse-vcsv "xx.csv" 1 '("a" (1 2 3) "b" 2 "c" 3))
  ;; (parse-vcsv "xx.csv" '(1 2) '("a" (1 2 3) "b" 2 "c" 3))

  (let ((cl-csv:*default-external-format* external-format)
        (key-number-p (if (typep key-row '(mod 1024)) t nil))
        (key-row (if (typep key-row '(mod 1024)) (list (- key-row 1)) (mapcar #'1- key-row)))
        (rc-hash-table (make-hash-table :test 'equal)))

    (typecase value-row
      ((mod 1024) (setf value-row (1- value-row)))
      (cons (typecase (nth 0 value-row)
              ((mod 1024) (setf value-row (mapcar #'1- value-row)))
              (t (progn
                   (loop for key in value-row
                         for i from 0
                         when (evenp i)
                         do (let ((row (getf value-row key)))
                              (typecase row
                                ((mod 1024) (setf (getf value-row key) (1- row)))
                                (cons (setf (getf value-row key) (mapcar #'1- row))))))
                   (setf value-row (alexandria:plist-hash-table value-row)))))))
    
    (labels ((number-key (key row)
               (setf (gethash key rc-hash-table)
                     (if auto-parse-type-p
                         (funcall (symbol-function auto-parse-type-func) row)
                         (nth value-row row))))
             
             (join-key (key row)
               (setf (gethash key rc-hash-table)
                     (mapcar #'(lambda (x) (if auto-parse-type-p
                                               (funcall (symbol-function auto-parse-type-func) row)
                                               (nth x row))) value-row)))

             (hash-key (key row)
               (let ((hash-value (make-hash-table :test 'equal)))
                 (maphash #'(lambda (key val)
                              (setf (gethash key hash-value)
                                    (typecase val
                                      ((mod 1024) (if auto-parse-type-p
                                                      (funcall (symbol-function auto-parse-type-func) row)
                                                      (nth val row)))
                                      (cons (loop for x in val
                                                  collect (if auto-parse-type-p
                                                              (funcall (symbol-function auto-parse-type-func) row)
                                                              (nth x row)))))
                                    )) value-row)
                 (setf (gethash key rc-hash-table) hash-value))))
        
      (cl-csv:do-csv (row (pathname filename) :skip-first-p skip-first-p)
        (let ((key (mapcar #'(lambda (x) (nth x row)) key-row)))
          (if key-number-p (setf key (first key)))       
          (typecase value-row
            ((mod 1024) (number-key key row))
            (cons (join-key key row))
            (hash-table (hash-key key row))))))
    
    rc-hash-table))
