;; Dawit Tsigie
;; 3D Vector Modelling & Serpienski's gasket


;; constructor procedures
(define (vector3D x y z)
  (list x y z ))

;;accessor procedures
(define (vector3D-get-x v)
  (list-ref v 0))
(define (vector3D-get-y v)
  (list-ref v 1))
(define (vector3D-get-z v)
  (list-ref v 2))

;; add procedure
(define (vector3D-add v u)
  (list (+ (list-ref v 0) (list-ref u 0))
        (+ (list-ref v 1) (list-ref u 1))
        (+ (list-ref v 2) (list-ref u 2))))

;; subtract procedure
(define (vector3D-sub v u)
  (list (- (list-ref v 0) (list-ref u 0))
        (- (list-ref v 1) (list-ref u 1))
        (- (list-ref v 2) (list-ref u 2))))

;; scale procedure
(define (vector3D-scale scale-factor v)
  (list (* (list-ref v 0) scale-factor)
        (* (list-ref v 1) scale-factor)
        (* (list-ref v 2) scale-factor)
        ))

;; dot product procedure
(define (vector3D-dot u v)
  (+   (* (list-ref v 0) (list-ref u 0))
       (* (list-ref v 1) (list-ref u 1))
       (* (list-ref v 2) (list-ref u 2))
       ))

;; magnitude procedure
(define (vector3D-magnitude v)
  (/ ( round (* 100 (sqrt (+ (expt (list-ref v 0) 2)
                             (expt (list-ref v 1) 2)
                             (expt (list-ref v 2) 2)))
                )) 100 ))

;; normalize method
(define (vector3D-normalize v)
  ( list (/ (round (* 100 (/ (list-ref v 0) ( vector3D-magnitude v)))) 100)
         (/ (round (* 100 (/ (list-ref v 1) ( vector3D-magnitude v)))) 100)
         (/ (round (* 100 (/ (list-ref v 2) ( vector3D-magnitude v)))) 100)
         ))
;; Cross product method
(define (vector3D-cross u v)
  (list (- (* (list-ref u 1) (list-ref v 2)) (* (list-ref u 2) (list-ref v 1)))
        (- (* (list-ref u 2) (list-ref v 0)) (* (list-ref u 0) (list-ref v 2)))
        (- (* (list-ref u 0) (list-ref v 1)) (* (list-ref u 1) (list-ref v 0)))
        ))

(define (df num)
  (number->string (/ (round ( * num 100))100)))

;; String representation for vector3D
(define (vector3D->string v)
  (string-append 
   "<"
   (df (vector3D-get-x v))
   ", "
   (df (vector3D-get-y v))
   ", "
   (df (vector3D-get-z v))
   ">"))

;;test vectors ( vertices of the triangles)
(define u (vector3D 1 0 0))
(define v (vector3D 0 1 0))
(define w (vector3D 0 0 1))

(define x (vector3D -1 0 0))
(define y (vector3D 0 -1 0))
(define z (vector3D 0 0 -1))

;; triangle constructor
(define (triangle p0 p1 p2)
  (list p0 p1 p2))

(define (triangle-get-p0 t)
  (list-ref t 0))
(define (triangle-get-p1 t)
  (list-ref t 1))
(define (triangle-get-p2 t)
  (list-ref t 2))

;; top triangles
(define a0 (triangle u v w ))
(define a1 (triangle v x w ))
(define a2 (triangle x y w ))
(define a3 (triangle y u w ))

;;bottom triangles
(define b0 (triangle u v z ))
(define b1 (triangle v x z ))
(define b2 (triangle x y z ))
(define b3 (triangle y u z ))

;;midpoint of two 3d vectors
(define (vector3D-midpoint p0 p1)
  (vector3D-scale 0.5 (vector3D-add p0 p1)))

;; define a function whose argument is a triangle
;; and that returns a list of three smaller triangles
;; constructed using the given triangle's 3 vertices
;; and the midpoints of its 3 edges, note the triangle found in between the three triangles has been ommitted for drawing the serpienski triangle
(define (smaller-triangles big-triangle)
  (let ((p0 (triangle-get-p0 big-triangle))
        (p1 (triangle-get-p1 big-triangle))
        (p2 (triangle-get-p2 big-triangle)))
    (let ((m01 (vector3D-midpoint p0 p1))
          (m12 (vector3D-midpoint p1 p2))
          (m20 (vector3D-midpoint p2 p0)))
      (list 
       (triangle p0 m01 m20)
       (triangle m01 p1 m12)
       (triangle m20 m12 p2))
      )))

;; define a function that recursively divides a
;; triangle into smaller triangles until the size
;; of the triangles falls below some threshold


(define (distance p1 p2) 
  (let ((delt-x(- (vector3D-get-x p1) (vector3D-get-x p2)))
        (delt-y(- (vector3D-get-y p1) (vector3D-get-y p2)))
        (delt-z(- (vector3D-get-z p1) (vector3D-get-z p2))))
    (sqrt(+(expt delt-x 2) (expt delt-y 2) (expt delt-z 2)))))
(define (triangle-size t)
  (let ((p0 (triangle-get-p0 t))
        (p1 (triangle-get-p1 t))
        (p2 (triangle-get-p2 t)))
    (min
     (distance p0 p1)
     (distance p1 p2)
     (distance p2 p0))))

;; subdivide recursively into 4 triangles with the triangle in the middle not saved
(define (subdivide t threshold)
  (if (< (triangle-size t) threshold)
      (list t)
      (let ((smaller (smaller-triangles t)))
        (append
         (subdivide (list-ref smaller 0 ) threshold)
         (subdivide (list-ref smaller 1 ) threshold)
         (subdivide (list-ref smaller 2 ) threshold)))))




;; converts triangle to povray format
(define (triangle->string t)
  (string-append
   
   " triangle {" "\n"
   "\t"   (vector3D->string (list-ref t 0)) ","
   "\n"
   "\t"  (vector3D->string (list-ref t 1)) ", "
   "\n"
   "\t"  (vector3D->string (list-ref t 2)) 
   
   "\t" "\n" " texture { Green }"  "\n"
   "} " "\n"
   ))

;;(map triangle->string (subdivide a0 0.5) )



;; describe the location and orientation of the camera
;; describe the location of 2 lights
;; define a texture
(define prefix
  (list
   "#include \"colors.inc\"" 
   "#include \"stones.inc\"" 
   "global_settings { assumed_gamma 1.0 }" 
   "camera {" 
   
   "location <0.0, 3.0, 0.0>"
   "look_at <0.0, 0.0, 0.0>"
   "}"
   "light_source { <2, 2, 8> color White }"
   "light_source { <-1, -1, -8> color White }"
   
   ;; some shade of purple
   "#declare Green = texture {"
   "  pigment {color rgb<0.3,0.6,0.8>}"
   "  finish {ambient 0.2 diffuse 0.5}"
   "}" 
   "#declare Red = texture {"
   "  pigment {color rgb<1.0,0.0,0.0>}"
   "  finish {ambient 0.2 diffuse 0.5}"
   "}" 
   
   ))

;; describes the 4 sides of the pyramidal top of an octahedron
(define octahedron-top
  (list 
   "mesh {"
   
   (apply string-append (map triangle->string (subdivide a0 0.1) ) )
   (apply string-append (map triangle->string (subdivide a1 0.1) ) )
   (apply string-append (map triangle->string (subdivide a2 0.1) ) )
   (apply string-append (map triangle->string (subdivide a3 0.1) ) )
   
   "}"
   ))

;; describes the 4 sides of the pyramidal bottom of an octahedron
(define octahedron-bottom
  (list
   "mesh {"
   "   triangle {"
   "    <1.0, 0.0, 0.0>,"
   "    <0.0, 1.0, 0.0>,"
   "    <0.0, 0.0, -1.0>"
   "    texture { Green }"
   "  }"
   "  triangle {"
   "    <0.0, 1.0, 0.0>,"
   "    <-1.0, 0.0, 0.0>,"
   "    <0.0, 0.0, -1.0>"
   "    texture { Green }"
   "  }"
   "  triangle {"
   "    <-1.0, 0.0, 0.0>,"
   "    <0.0, -1.0, 0.0>,"
   "    <0.0, 0.0, -1.0>"
   "    texture { Green }"
   "  }"
   "  triangle {"
   "    <0.0, -1.0, 0.0>,"
   "    <1.0, 0.0, 0.0>,"
   "    <0.0, 0.0, -1.0>"
   "    texture { Green }"
   "  }"
   "}"
   ))

;; describe the square base of a pyramid
(define base
  (list
   "polygon {"
   "  4,"
   "  <1.0, 0.0, 0.0>,"
   "  <0.0, 1.0, 0.0>,"
   "  <-1.0, 0.0, 0.0>,"
   "  <0.0, -1.0, 0.0>"
   "  texture { Green }"
   "}"
   ))



;; Here's an example to show how to 
;; convert a number to a string.
(define (n2s n) (number->string n))

;; file-name is a string (it must name a file that does not exist)
;; file-lines is a list of strings (lines to be written to the file)
(define (my-io file-name file-lines)
  (call-with-output-file file-name
    (lambda (output-port)
      (display 
       (apply string-append (map (lambda (x) (string-append x "\n")) file-lines))
       output-port))))

;; run by typing Dr Racket: (my-io "mypov.pov" (append prefix octahedron-top base))
;; then display by typing in the terminal window:
;; povray +Imypov.pov Width=512 Height=384 Display=true +P

