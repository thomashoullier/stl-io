;;;; Rove test suite for stl-io.
(defpackage :stl-io/test
  (:documentation "Rove test suite for stl-io.")
  (:use :cl :rove))

(in-package :stl-io/test)

(defun read-write-compare (file-in)
  "Read a stl file. Write it back to another file. Compare the two to
   check that we have the expected bit-perfect match between the two.
   T if the files are the same. nil otherwise."
  (let ((stl-struct)
        (temp-file (uiop:tmpize-pathname
                    (merge-pathnames (uiop:temporary-directory)
                                     "stl-copy.stl")))
        (orig-bytes)
        (copy-bytes)
        (same-file T))
    ;; Read the stl file into a struct.
    (setf stl-struct (stl-io:read-stl file-in))
    ;; Write it back to a temporary file.
    (stl-io:write-stl stl-struct temp-file)
    ;; Compare the two files.
    (psetf orig-bytes (alexandria:read-file-into-byte-vector file-in)
           copy-bytes (alexandria:read-file-into-byte-vector temp-file))
    (assert (< 0 (length copy-bytes)))
    (block valid (loop for orig across orig-bytes
                       for cpy across copy-bytes do
                         (when (/= orig cpy)
                           (setf same-file nil)
                           (return-from valid))))
    same-file))

(deftest read-write-check
  (testing "cube"
    (ok (read-write-compare
         (merge-pathnames #P"test/data/cube-bin.stl"
                          (asdf:system-source-directory "stl-io")))
        "Same file as read.")))
