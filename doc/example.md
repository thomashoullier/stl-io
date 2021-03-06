# stl-io examples

## Reading a stl file
Let us read the binary stl file ![cube-bin.stl](../test/data/cube-bin.stl) into
a `stl-bin` structure.

```common-lisp
(defparameter *stl-file-in*
  (merge-pathnames #P"test/data/cube-bin.stl"
                   (asdf:system-source-directory "stl-io")))

(defparameter *stl-struct* (stl-io:read-stl *stl-file-in*))
```

We get the structure:

```common-lisp
 #S(STL-IO:STL-BIN
    :HEADER "Exported from Blender-2.83.0                                                    "
    :NFACETS 12
    :FACETS #(#S(STL-IO:FACET
                 :NVEC #(0.0 0.0 1.0)
                 :V1 #(1.0 1.0 1.0)
                 :V2 #(-1.0 1.0 1.0)
                 :V3 #(-1.0 -1.0 1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 0.0 1.0)
                 :V1 #(1.0 1.0 1.0)
                 :V2 #(-1.0 -1.0 1.0)
                 :V3 #(1.0 -1.0 1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 -1.0 0.0)
                 :V1 #(1.0 -1.0 -1.0)
                 :V2 #(1.0 -1.0 1.0)
                 :V3 #(-1.0 -1.0 1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 -1.0 0.0)
                 :V1 #(1.0 -1.0 -1.0)
                 :V2 #(-1.0 -1.0 1.0)
                 :V3 #(-1.0 -1.0 -1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(-1.0 0.0 0.0)
                 :V1 #(-1.0 -1.0 -1.0)
                 :V2 #(-1.0 -1.0 1.0)
                 :V3 #(-1.0 1.0 1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(-1.0 0.0 0.0)
                 :V1 #(-1.0 -1.0 -1.0)
                 :V2 #(-1.0 1.0 1.0)
                 :V3 #(-1.0 1.0 -1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 0.0 -1.0)
                 :V1 #(-1.0 1.0 -1.0)
                 :V2 #(1.0 1.0 -1.0)
                 :V3 #(1.0 -1.0 -1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 0.0 -1.0)
                 :V1 #(-1.0 1.0 -1.0)
                 :V2 #(1.0 -1.0 -1.0)
                 :V3 #(-1.0 -1.0 -1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(1.0 0.0 0.0)
                 :V1 #(1.0 1.0 -1.0)
                 :V2 #(1.0 1.0 1.0)
                 :V3 #(1.0 -1.0 1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(1.0 0.0 0.0)
                 :V1 #(1.0 1.0 -1.0)
                 :V2 #(1.0 -1.0 1.0)
                 :V3 #(1.0 -1.0 -1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 1.0 0.0)
                 :V1 #(-1.0 1.0 -1.0)
                 :V2 #(-1.0 1.0 1.0)
                 :V3 #(1.0 1.0 1.0)
                 :ATTR 0)
              #S(STL-IO:FACET
                 :NVEC #(0.0 1.0 0.0)
                 :V1 #(-1.0 1.0 -1.0)
                 :V2 #(1.0 1.0 1.0)
                 :V3 #(1.0 1.0 -1.0)
                 :ATTR 0)))
```

## Creating a mesh from scratch and writing to a stl file
We will write a tetrahedron to a stl file. First let us create an
array of points for the individual triangular faces. We follow
a right hand rule on the order of points so the normals will point
out of the solid.

```common-lisp
(defparameter *faces*
  #(#(#(0.0 0.0 0.0) #(0.0 1.0 0.0) #(1.0 0.0 0.0))
    #(#(0.0 0.0 0.0) #(1.0 0.0 0.0) #(0.0 0.0 1.0))
    #(#(0.0 0.0 0.0) #(0.0 0.0 1.0) #(0.0 1.0 0.0))
    #(#(0.0 0.0 1.0) #(1.0 0.0 0.0) #(0.0 1.0 0.0))))

;; There is some type marshalling to perform: the input
;; must be simple-arrays of single-floats.
;; (There might be a smarter way to go about all this.)
(loop for face across *faces*
      for i from 0 do
        (loop for point across face
              for ipoint from 0 do
                (setf (aref (aref *faces* i) ipoint)
                      (coerce point '(simple-array single-float (3))))))
```

Now we fill a `'facet` array with these points, computing the normals along
the way:

```common-lisp
(defparameter *facets* (make-array 4))
(loop for face across *faces*
      for iface from 0
      for v1 = (aref face 0)
      for v2 = (aref face 1)
      for v3 = (aref face 2) do
        (setf (aref *facets* iface)
              (stl-io:make-facet
               :v1 v1 :v2 v2 :v3 v3
               :nvec (stl-io:nvec v1 v2 v3))))
```

Then we create the `'stl-bin` struct:

```common-lisp
(defparameter *tetra*
  (stl-io:make-stl-bin
   :header "Tetrahedron written with stl-io."
   :nfacets (length *facets*)
   :facets (coerce *facets* 'simple-array)))
```

And finally we write it to a file:

```common-lisp
(stl-io:write-stl *tetra*
                  (uiop:tmpize-pathname
                   (merge-pathnames (uiop:temporary-directory)
                                    "tetrahedron.stl")))
```

We can check the result by importing the generated stl file into (eg.) Blender:

![tetrahedron-blender](tetrahedron.png)
