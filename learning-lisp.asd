(defsystem "learning-lisp"
  :version "0.0.1"
  :author "HOWZ1T@github.com"
  :license "GNU GPLv3"
  :depends-on ()
  :components ((:module "src"
			:components ((:module "utils"
					      :components ((:file "general")))
				     (:module "db"
					      :depends-on ("utils")
					      :components ((:file "db")))
				     (:file "main"))))
  :description "A project playground for learning lisp"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
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
