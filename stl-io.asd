(defsystem stl-io
  :name "stl-io"
  :version "0.1"
  :author "Thomas HOULLIER"
  :description "Reading and writing .stl files."
  :depends-on ("lisp-binary")
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "defbinary" :depends-on ("package"))
                 (:file "stl-io" :depends-on ("package" "defbinary"))))))
