Vectors, Matrices, and Serpienski's Gasket in Scheme
Dawit Tsigie
CSC 315
10/11/13

files included: vector3D.scm & matrix.scm 

Vectors- I implemented different methods in the vector3D program for different types of vector calculations such as a dot product method, a cross product method, a magnitude method, a difference method, a normalize method and a scale vector method. 

Matrices - I implemented different  methods such as a constructor for a scaling matrix, a method which finds the three different rotation matrices, and matrix by matrix multiplication method. 

Serpienski's Gasket- I used the top half of the octahedron to create a 3D version of the gasket. I used a method to recursively subdivide the 4 faces(triangles) of the pyramid into smaller triangles. And used a method that converts triangle into strings that are appropriate for the input of the povray.

How to run

1. matrix.scm - test the various methods by typing the methods in the command window with the defined test values as the arguments. 


2. vector3D.scm - test the various methods by typing the methods in the command with the defined test values as the arguments.

3. vector3D.scm (Serpienski's gasket) - type in the command window of DrRacket: 

(my-io "mypov.pov" (append prefix octahedron-top base))

then display by typing in the terminal window:

 povray +Imypov.pov Width=512 Height=384 Display=true +P

