;;;; Defining useful structures.
(in-package :stl-io)

(defbinary facet (:byte-order :little-endian)
           (nvec #() :type (simple-array single-float (3)))
           (v1 #() :type (simple-array single-float (3)))
           (v2 #() :type (simple-array single-float (3)))
           (v3 #() :type (simple-array single-float (3)))
           (attr 0 :type (unsigned-byte 16)))

(defbinary stl-bin (:byte-order :little-endian)
           (header "" :type (fixed-length-string 80))
           (nfacets 0 :type (unsigned-byte 32))
           (facets #() :type (simple-array facet (nfacets))))

(defparameter *stream*
  (open-binary "~/repos/cl/stl-io/test/data/cube-bin.stl"))

(defparameter *cube* (read-binary 'stl-bin *stream*))
