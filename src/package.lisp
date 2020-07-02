(defpackage :stl-io
  (:documentation "Reading and writing .stl files.")
  (:use :cl :lisp-binary)
  (:export #:read-stl #:write-stl
           #:nvec))
