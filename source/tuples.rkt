#lang racket

(provide color
         vector
         point
         vector-sum
         vector+c
         vector*c
         vector/c
         -vector
         vector+
         vector-
         vector*
         dot
         magnitude
         normalize
         cross
         point+c
         point*c
         point/c
         -point
         point+vector
         point-vector
         point-
         color
         color+c
         color*c
         color/c
         -color
         color+
         color-
         color*
         black
         white
         red
         green
         blue)

(require "utilities.rkt")

;;**********************************************************
;; Datatypes
;;**********************************************************

(struct color (red green blue) #:transparent)

(struct vector (i j k) #:transparent)

(struct point (x y z) #:transparent)


;;**********************************************************
;; Tuple
;;**********************************************************

(define tuple?
  (make-flat-contract #:name 'tuple?
                      #:first-order (or/c vector? point? color?)))

(define/contract (tuple->list tuple)
  (-> tuple? list?)
  (match tuple
    [(struct vector (i j k)) (list i j k)]
    [(struct point (x y z)) (list x y z)]
    [(struct color (r g b)) (list r g b)]))

(define/contract (map-elementwise proc tuple)
  (-> (-> real? real?) tuple? tuple?)
  (match tuple
    [(struct vector (i j k)) (vector (proc (vector-i tuple))
                                     (proc (vector-j tuple))
                                     (proc (vector-k tuple)))]
    [(struct point (x y z)) (point (proc (point-x tuple))
                                   (proc (point-y tuple))
                                   (proc (point-z tuple)))]
    [(struct color (r g b)) (color (proc (color-red tuple))
                                   (proc (color-green tuple))
                                   (proc (color-blue tuple)))]))

(define/contract (map-pairwise proc tuple1 tuple2)
  (-> (-> real? real? real?) tuple? tuple? tuple?)
  (match (list tuple1 tuple2)
    [(list (struct vector (i1 j1 k1)) (struct vector (i2 j2 k2)))
     (vector (proc i1 i2) (proc j1 j2) (proc k1 k2))]
    [(list (struct point (x1 y1 z1)) (struct point (x2 y2 z2)))
     (point (proc x1 x2) (proc y1 y2) (proc z1 z2))]
    [(list (struct color (r1 g1 b1)) (struct color (r2 g2 b2)))
     (color (proc r1 r2) (proc g1 g2) (proc b1 b2))]))

(define/contract (compare-tuple tuple1 tuple2)
  (-> tuple? tuple? boolean?)
  (match (list tuple1 tuple2)
    [(list (struct vector (i1 j1 k1)) (struct vector (i2 j2 k2)))
     (and (compare-float i1 i2) (compare-float j1 j2) (compare-float k1 k2))]
    [(list (struct point (x1 y1 z1)) (struct point (x2 y2 z2)))
     (and (compare-float x1 x2) (compare-float y1 y2) (compare-float z1 z2))]
    [(list (struct color (r1 g1 b1)) (struct color (r2 g2 b2)))
     (and (compare-float r1 r2) (compare-float g1 g2) (compare-float b1 b2))]))

(define (tuple-op-by-constant op tuple c)
  (map-elementwise (λ (e) (op e c)) tuple))

(define (tuple-op-pairwise op t1 t2 . tuples)
  (cond [(empty? tuples) (map-pairwise op t1 t2)]
        [else (apply tuple-op-pairwise op
                     (map-pairwise op t1 t2)
                     (car tuples)
                     (cdr tuples))]))


;;**********************************************************
;; Vector
;;**********************************************************

(define (vector-sum v)
  (apply + (tuple->list v)))

(define (vector+c v c)
  (tuple-op-by-constant + v c))

(define (vector*c v c)
  (tuple-op-by-constant * v c))

(define (vector/c v c)
  (tuple-op-by-constant / v c))

(define (-vector v)
  (vector*c v -1))

(define (vector+ u v . vectors)
  (apply tuple-op-pairwise + u v vectors))

(define (vector- u v . vectors)
  (apply tuple-op-pairwise - u v vectors))

(define (vector* u v . vectors)
  (apply tuple-op-pairwise * u v vectors))

(define (dot-product u v)
  (vector-sum (vector* u v)))

(define dot dot-product)

(define (magnitude v)
  (sqrt (dot-product v v)))

(define (normalize v)
  (vector/c v (magnitude v)))

(define (cross-product u v)
  (vector (- (* (vector-j u) (vector-k v))
             (* (vector-j v) (vector-k u)))
          (- (* (vector-i v) (vector-k u))
             (* (vector-i u) (vector-k v)))
          (- (* (vector-i u) (vector-j v))
             (* (vector-i v) (vector-j u)))))

(define cross cross-product)  


;;**********************************************************
;; Point
;;**********************************************************

(define (point+c p c)
  (tuple-op-by-constant + p c))

(define (point*c p c)
  (tuple-op-by-constant * p c))

(define (point/c p c)
  (tuple-op-by-constant / p c))

(define (-point p)
  (point*c p -1))

(define/contract (point+vector p v)
  (-> point? vector? point?)
  (point (+ (point-x p) (vector-i v))
         (+ (point-y p) (vector-j v))
         (+ (point-z p) (vector-k v))))

(define/contract (point-vector p v)
  (-> point? vector? point?)
  (point (- (point-x p) (vector-i v))
         (- (point-y p) (vector-j v))
         (- (point-z p) (vector-k v))))

(define/contract (point- p q . points)
  (-> point? point? vector?)
  (vector (- (point-x p) (point-x q))
          (- (point-y p) (point-y q))
          (- (point-z p) (point-z q))))


;;**********************************************************
;; Color
;;**********************************************************

(define (color+c color c)
  (tuple-op-by-constant + color c))

(define (color*c color c)
  (tuple-op-by-constant * color c))

(define (color/c color c)
  (tuple-op-by-constant / color c))

(define (-color c)
  (color*c c -1))

(define (color+ c1 c2 . colors)
  (apply tuple-op-pairwise + c1 c2 colors))

(define (color- c1 c2 . colors)
  (apply tuple-op-pairwise - c1 c2 colors))

(define (color* c1 c2 . colors)
  (apply tuple-op-pairwise * c1 c2 colors))

(define black (color 0 0 0))
(define white (color 1 1 1))
(define red (color 1 0 0))
(define green (color 0 1 0))
(define blue (color 0 0 1))


;;**********************************************************
;; Tests
;;**********************************************************

(module+ test
  (require rackunit)

  ;; "A tuple with w=1.0 is a point"
  ;; "A tuple with w=0 is a vector"
  ;; These tests are not implemented because points and vectors are two distinct types in
  ;; this implementation.

  (test-equal? "point creates tuples"
               (let ([p (point 4 -4 3)])
                 (list (point-x p) (point-y p) (point-z p)))
               (list 4 -4 3))

  (test-equal? "vector creates tuples"
               (let ([v (vector 4 -4 3)])
                 (list (vector-i v) (vector-j v) (vector-k v)))
               (list 4 -4 3))

  (test-equal? "Adding a vector to a point" ; original test name: "Adding two tuples"
               (point+vector (point 3 -2 5) (vector -2 3 1))
               (point 1 1 6))

  (test-equal? "Adding a vector to a vector" ; original test name: "Adding two tuples"
               (vector+ (vector 1 2 3) (vector 4 5.5 6.5))
               (vector 5 7.5 9.5))

  (test-equal? "Subtracting two points"
               (point- (point 3 2 1) (point 5 6 7))
               (vector -2 -4 -6))

  (test-equal? "Subtracting a vector from a point"
               (point-vector (point 3 2 1) (vector 5 6 7))
               (point -2 -4 -6))

  (test-equal? "Subtracting two vectors"
               (vector- (vector 3 2 1) (vector 5 6 7))
               (vector -2 -4 -6))

  (test-equal? "Subtracting a vector from the zero vector"
               (vector- (vector 0 0 0) (vector 1 -2 3))
               (vector -1 2 -3))

  (test-equal? "Negating a vector" ; original test name: "Negating a tuple"
               (-vector (vector 1 -2 3))
               (vector -1 2 -3))

  (test-equal? "Negating a point" ; original test name: "Negating a tuple"
               (-point (point 1 -2 3))
               (point -1 2 -3))

  (test-check "Multiplying a point by a scalar" ; original test name: "Multiplying a tuple by a scalar"
              compare-tuple
              (point*c (point 1 -2 3) 3.5)
              (point 3.5 -7 10.5))

  (test-check "Multiplying a vector by a scalar" ; original test name: "Multiplying a tuple by a scalar"
              compare-tuple
              (vector*c (vector 1 -2 3) 3.5)
              (vector 3.5 -7 10.5))

  (test-check "Multiplying a point by a fraction" ; original test name: "Multiplying a tuple by a fraction"
              compare-tuple
              (point*c (point 1 -2 3) 0.5)
              (point 0.5 -1 1.5))

  (test-check "Multiplying a vector by a fraction" ; // original test name: "Multiplying a tuple by a fraction"
              compare-tuple
              (vector*c (vector 1 -2 3) 0.5)
              (vector 0.5 -1 1.5))

  (test-check "Dividing a point by a scalar" ; original test name: "Dividing a tuple by a scalar"
              compare-tuple
              (point/c (point 1 -2 3) 2)
              (point 0.5 -1 1.5))

  (test-check "Dividing a vector by a scalar" ; original test name: "Dividing a tuple by a scalar"
              compare-tuple
              (vector/c (vector 1 -2 3) 2)
              (vector 0.5 -1 1.5))

  (test-check "Computing the magnitude of vector(1.0, 0.0, 0.0)"
              compare-float
              (magnitude (vector 1 0 0))
              1)

  (test-check "Computing the magnitude of vector(0.0, 1.0, 0.0)"
              compare-float
              (magnitude (vector 0 1 0))
              1)

  (test-check "Computing the magnitude of vector(0.0, 0.0, 1.0)"
              compare-float
              (magnitude (vector 0 0 1))
              1)

  (test-check "Computing the magnitude of vector(1.0, 2.0, 3.0)"
              compare-float
              (magnitude (vector 1 2 3))
              (sqrt 14))

  (test-check "Computing the magnitude of vector(-1.0, -2.0, -3.0)"
              compare-float
              (magnitude (vector -1 -2 -3))
              (sqrt 14))

  (test-check "Normalizing vector(4.0, 0.0, 0.0) gives (1.0, 0.0, 0.0)"
              compare-tuple
              (normalize (vector 4.0 0 0))
              (vector 1 0 0))

  (test-check "Normalizing vector(1.0, 2.0, 3.0)"
              compare-tuple
              (normalize (vector 1 2 3))
              (vector (/ 1 (sqrt 14)) (/ 2 (sqrt 14)) (/ 3 (sqrt 14))))

  (test-check "The magnitude of a normalized vector"
              compare-float
              (magnitude (normalize (vector 1 2 3)))
              1)

  (test-check "The dot product of two vectors"
              compare-float
              (dot (vector 1 2 3) (vector 2 3 4))
              20)

  (define a (vector 1 2 3))
  (define b (vector 2 3 4))

  (test-check "The cross product of two vectors"
              compare-tuple
              (cross a b)
              (vector -1 2 -1))

  (test-check "The cross product of two vectors"
              compare-tuple
              (cross b a)
              (vector 1 -2 1))

  (test-equal? "Colors are (red, green, blue) tuples"
               (let ([c (color -0.5 0.4 1.7)])
                 (list (color-red c) (color-green c) (color-blue c)))
               (list -0.5 0.4 1.7))

  (test-check "Adding colors"
              compare-tuple
              (color+ (color 0.9 0.6 0.75) (color 0.7 0.1 0.25))
              (color 1.6 0.7 1.0))

  (test-check "Subtracting colors"
              compare-tuple
              (color- (color 0.9 0.6 0.75) (color 0.7 0.1 0.25))
              (color 0.2 0.5 0.5))

  (test-check "Multiplying a color by a scalar"
              compare-tuple
              (color*c (color 0.2 0.3 0.4) 2)
              (color 0.4 0.6 0.8))

  (test-check "Multiplying colors"
              compare-tuple
              (color* (color 1.0 0.2 0.4) (color 0.9 1.0 0.1))
              (color 0.9 0.2 0.04))
  
  )