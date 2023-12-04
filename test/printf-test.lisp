(defpackage kit-printf/test
  (:use #:cl #:kit)
  (:export test))

(in-package #:kit-printf/test)

(defun test ()
  (let ((x (make-hash-table)))
    (setf (gethash "a" x) 1)
    (setf (gethash "b" x) 1)
    (setf (gethash "c" x) 1)
    (setf (gethash "d" x) 1)
    (setf (gethash "e" x) (make-hash-table))
    (setf (gethash "f" x) (make-hash-table))
    (setf (gethash "g" x) (list 1 2 3 4 5 6 7))
    (setf (gethash "h" x) '((1 2) (3 4) (6 7)))
    (setf (gethash "i" x) (make-hash-table))
    (setf (gethash "j" x) (make-hash-table))
    (setf (gethash "k" x) (list (make-hash-table) (make-hash-table) (make-hash-table)))

    (setf (gethash "a" (gethash "e" x)) 1)
    (setf (gethash "b" (gethash "e" x)) 2)
    (setf (gethash "c" (gethash "e" x)) 3)
    (setf (gethash "d" (gethash "e" x)) 4)
    (setf (gethash "e" (gethash "e" x)) 5)

    (setf (gethash "a" (gethash "f" x)) 1)
    (setf (gethash "b" (gethash "f" x)) 2)
    (setf (gethash "c" (gethash "f" x)) 3)
    (setf (gethash "d" (gethash "f" x)) 4)
    (setf (gethash "e" (gethash "f" x)) 5)

    (setf (gethash "a" (gethash "j" x)) 1)
    (setf (gethash "b" (gethash "j" x)) 2)
    (setf (gethash "c" (gethash "j" x)) 3)
    (setf (gethash "d" (gethash "j" x)) 4)
    (setf (gethash "e" (gethash "j" x)) 5)

    (setf (gethash "a" (nth 0 (gethash "k" x))) 1)
    (setf (gethash "b" (nth 0 (gethash "k" x))) 2)
    (setf (gethash "c" (nth 0 (gethash "k" x))) 3)
    (setf (gethash "d" (nth 0 (gethash "k" x))) 4)
    (setf (gethash "e" (nth 0 (gethash "k" x))) 5)

    (setf (gethash "a" (nth 1 (gethash "k" x))) 1)
    (setf (gethash "b" (nth 1 (gethash "k" x))) 2)
    (setf (gethash "c" (nth 1 (gethash "k" x))) 3)
    (setf (gethash "d" (nth 1 (gethash "k" x))) 4)
    (setf (gethash "e" (nth 1 (gethash "k" x))) 5)
    
    (setf (gethash "a" (nth 2 (gethash "k" x))) 1)
    (setf (gethash "b" (nth 2 (gethash "k" x))) 2)
    (setf (gethash "c" (nth 2 (gethash "k" x))) 3)
    (setf (gethash "d" (nth 2 (gethash "k" x))) 4)
    (setf (gethash "e" (nth 2 (gethash "k" x))) '(1 2 3 4 5))

    (let ((*line-width* 2))
      (printf x))))

