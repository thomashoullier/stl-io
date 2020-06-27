;;;; Defining useful structures.
(in-package :stl-io)

(defstruct solid
  (name "unspecified")
  facets ; vector of facets.
  )

(defstruct facet
  (n #(0e0 0e0 0e0)) ; normal vector, xyz
  verts ; vector of vertices #(#(x1 y1 z1) #(x2 y2 z2) #(x3 y3 z3) ...)
  )
