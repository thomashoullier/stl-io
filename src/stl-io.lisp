(in-package :stl-io)

(defun write-stl (stl-struct file-out)
  "Write a stl-bin struct into a stl file."
  (with-open-binary-file (binstr file-out
                                 :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
    (write-binary stl-struct binstr)))

(defun read-stl (file-in)
  "Read from a stl file file-in.
   Returns an stl-bin struct."
  (with-open-binary-file (binstr file-in)
    (read-binary 'stl-bin binstr)))
