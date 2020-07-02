;;;; Helper functions
(in-package :stl-io)

(defun vec-norm (v)
  "Compute the euclidian norm of a 3-vector."
  (sqrt (+ (* (aref v 0) (aref v 0))
           (* (aref v 1) (aref v 1))
           (* (aref v 2) (aref v 2)))))

(defun nvec (v1 v2 v3)
  "Compute a normal vector #[x y z] from the vertices v1, v2 and v3
   according to the right hand rule with the order of vertices.
   v1, v2 and v3 should be vectors of single-floats.
   Reminder: nvec = norm[ [v2 - v1] x [v3 - v1] ], with 'x' the cross-product.
             u = v2 - v1 ; v = v3 - v1"
  (let ((norm-vec (make-array 3 :element-type 'single-float))
        (ux (- (aref v2 0) (aref v1 0)))
        (uy (- (aref v2 1) (aref v1 1)))
        (uz (- (aref v2 2) (aref v1 2)))
        (vx (- (aref v3 0) (aref v1 0)))
        (vy (- (aref v3 1) (aref v1 1)))
        (vz (- (aref v3 2) (aref v1 2)))
        (norm 0.0))
    (psetf (aref norm-vec 0) (- (* uy vz) (* uz vy))
           (aref norm-vec 1) (- (* uz vx) (* ux vz))
           (aref norm-vec 2) (- (* ux vy) (* uy vx)))
    ;; Normalize
    (setf norm (vec-norm norm-vec))
    (psetf (aref norm-vec 0) (/ (aref norm-vec 0) norm)
           (aref norm-vec 1) (/ (aref norm-vec 1) norm)
           (aref norm-vec 2) (/ (aref norm-vec 2) norm))
    norm-vec))
