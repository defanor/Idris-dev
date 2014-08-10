;;; -*- lexical-binding: t -*-

(setq idris-stack [])
(setq idris-base 0)
(setq idris-top 0)
(setq idris-call-stack (make-vector 0 '()))
(setq idris-call-stack-ptr -1)


(defsubst idris-reserve (n)
  (setq idris-stack (vconcat idris-stack (make-vector n '()))))

(defsubst idris-slide (n)
  (dotimes (num n t)
    (aset idris-stack (+ idris-base num) (aref idris-stack (+ idris-top num)))))

(defsubst idris-project (val n argn)
  (let ((args (cdr val)))
    (dotimes (num argn t)
      (aset idris-stack (+ idris-base n num) (nth num args)))))


(defsubst idris-call (func &rest args)
  (setq idris-call-stack-ptr (1+ idris-call-stack-ptr))
  (when (>= idris-call-stack-ptr (length idris-call-stack))
    (setq idris-call-stack (vconcat idris-call-stack (make-vector (* (1+ idris-call-stack-ptr) 2) '()))))
  (aset idris-call-stack idris-call-stack-ptr (cons func args)))

(defun idris-run ()
  (while (>= idris-call-stack-ptr 0)
    (setq idris-call-stack-ptr (1- idris-call-stack-ptr))
    (eval (aref idris-call-stack (1+ idris-call-stack-ptr)))))

(defsubst idris-run-main ()
  (setq idris-stack [])
  (setq idris-base 0)
  (setq idris-top 0)
  (setq idris-call-stack (make-vector 0 '()))
  (setq idris-call-stack-ptr -1)
  (idris-call '{runMain0} 0 0)
  (idris-run))


(defsubst putStr (str)
  (princ str))

