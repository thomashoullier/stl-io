(defsystem stl-io
  :name "stl-io"
  :version "0.1"
  :author "Thomas HOULLIER"
  :description "Reading and writing .stl files."
  ;; :depends-on ()
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "struct" :depends-on "package")
                 (:file "stl-io" :depends-on "package" "struct")))))
