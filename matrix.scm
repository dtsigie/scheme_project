;; Author: Dawit Tsigie
;; 10/12/2013
;;row major order matrix

;; row builder
(define (row  a b c)
  (list a b c))

;; matrix constructor
(define (matrix row1 row2 row3)
  (list row1 row2 row3))

;; element access , indices start at 1 1
(define (matrix-element matr a b)
  (cond ((and (= a 1) (= b 1))
         (list-ref (list-ref matr 0) 0))
        ((and (= a 1) (= b 2))
         (list-ref (list-ref matr 0) 1))
        ((and (= a 1) (= b 3))
         (list-ref (list-ref matr 0) 2))
        ((and (= a 2) (= b 1))
         (list-ref (list-ref matr 1) 0))
        ((and (= a 2) (= b 2))
         (list-ref (list-ref matr 1) 1))
        ((and (= a 2) (= b 3))
         (list-ref (list-ref matr 1) 2))
        ((and (= a 3) (= b 1))
         (list-ref (list-ref matr 2) 0))
        ((and (= a 3) (= b 2))
         (list-ref (list-ref matr 2) 1))
        ((and (= a 3) (= b 3))
         (list-ref (list-ref matr 2) 2))
        (else "Please enter correct indices")))

;; test matrix
(define row-1 (row 2 3 4))
(define row-2 (row 5 6 7))
(define row-3 (row 8 9 10))
(define matr (matrix row-1 row-2 row-3))

;; identity matrix 
(define (ident-matr) (matrix (row 1 0 0) (row 0 1 0) (row 0 0 1)))

;; rotation matrix about x-axis
(define (rot-matrX theta) (matrix (row 1 0 0) (row 0  (cos theta) (* -1 (sin theta))) (row 0 (sin theta) (cos theta))))

;; rotation matrix about y-axis
(define (rot-matrY theta) (matrix (row (cos theta) 0 (sin theta)) (row 0 1 0) (row (* -1 (sin theta)) 0 (cos theta))))

;; rotation matrix about z-axis
(define (rot-matrZ theta) (matrix (row (cos theta) (* -1 (sin theta)) 0) (row (sin theta) (cos theta) 0) (row 0 0 1)))

;; scaling matrix
(define (scale-matrix scale-factor) (matrix (row scale-factor 0 0) (row 0 scale-factor 0) (row 0 0 scale-factor)))

;; matrix x matrix multiplication
(define (matr-multi a b) (matrix (row (+ (* (matrix-element a 1 1) (matrix-element b 1 1)) (* (matrix-element a 1 2) (matrix-element b 2 1))
                                         (* (matrix-element a 1 3) (matrix-element b 3 1))) 
                                      (+ (* (matrix-element a 1 1) (matrix-element b 1 2)) (* (matrix-element a 1 2) (matrix-element b 2 2))
                                         (* (matrix-element a 1 3) (matrix-element b 3 2)))
                                      (+ (* (matrix-element a 1 1) (matrix-element b 1 3)) (* (matrix-element a 1 2) (matrix-element b 2 3))
                                         (* (matrix-element a 1 3) (matrix-element b 3 3))))
                                 
                                 (row (+ (* (matrix-element a 2 1) (matrix-element b 1 1)) (* (matrix-element a 2 2) (matrix-element b 2 1))
                                         (* (matrix-element a 2 3) (matrix-element b 3 1))) 
                                      (+ (* (matrix-element a 2 1) (matrix-element b 1 2)) (* (matrix-element a 2 2) (matrix-element b 2 2))
                                         (* (matrix-element a 2 3) (matrix-element b 3 2)))
                                      (+ (* (matrix-element a 2 1) (matrix-element b 1 3)) (* (matrix-element a 2 2) (matrix-element b 2 3))
                                         (* (matrix-element a 2 3) (matrix-element b 3 3))))
                                 
                                 (row (+ (* (matrix-element a 3 1) (matrix-element b 1 1)) (* (matrix-element a 3 2) (matrix-element b 2 1))
                                         (* (matrix-element a 3 3) (matrix-element b 3 1))) 
                                      (+ (* (matrix-element a 3 1) (matrix-element b 1 2)) (* (matrix-element a 3 2) (matrix-element b 1 1))
                                         (* (matrix-element a 3 3) (matrix-element b 3 2)))
                                      (+ (* (matrix-element a 3 1) (matrix-element b 1 3)) (* (matrix-element a 3 2) (matrix-element b 2 3))
                                         (* (matrix-element a 3 3) (matrix-element b 3 3))))))











