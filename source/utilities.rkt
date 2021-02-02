#lang racket

(provide epsilon
         compare-float
         square)

(define epsilon 0.00001)

(define (compare-float x y)
  (< (abs (- x y)) epsilon))

(define (square x)
  (* x x))