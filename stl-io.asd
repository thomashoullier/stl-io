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
                 (:file "stl-io" :depends-on ("package" "defbinary")))))
  :in-order-to ((test-op (test-op "stl-io/test"))))

(defsystem stl-io/test
  :name "stl-io/test"
  :version "0.1"
  :author "Thomas HOULLIER"
  :description "Rove test suite for stl-io."
  :depends-on ("stl-io" "rove" "alexandria" "uiop" "asdf")
  :components
  ((:module "test"
    :components ((:file "rove-suite"))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
