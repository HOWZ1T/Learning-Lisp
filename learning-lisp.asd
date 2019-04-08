(defsystem "learning-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "learning-lisp/tests"))))

(defsystem "learning-lisp/tests"
  :author ""
  :license ""
  :depends-on ("learning-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for learning-lisp"

  :perform (test-op (op c) (symbol-call :rove :run c)))
