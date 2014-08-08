;;; -*- lexical-binding: t -*-

(setq idris-stack [])
(setq idris-base 0)
(setq idris-top 0)

(defsubst idris-reserve (n)
  (setq idris-stack (vconcat idris-stack (make-vector n '()))))

(defsubst idris-slide (n)
  (dotimes (num n t)
    (aset idris-stack (+ idris-base num) (aref idris-stack (+ idris-top num)))))

(defsubst idris-project (val n argn)
  (let ((args (cdr val)))
    (dotimes (num argn t)
      (aset idris-stack (+ idris-base n num) (nth num args)))))

(defsubst putStr (str)
  (message str))

