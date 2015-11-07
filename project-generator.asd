(defsystem project-generator
  :version "0.1"
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:cl-mustache
               :trivial-types
               :split-sequence
               :anaphora
               :local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "generator")
                 (:file "util")
                 (:module "skeletons"
                  :components
                  ((:module "generic"
                    :serial t
                    :components
                    ((:static-file "asdf.lisp")
                     (:static-file "asdf-test.lisp")
                     (:static-file "README.md")
                     (:static-file "source.lisp")
                     (:static-file "test.lisp")
                     (:file "generic")))
                   (:module "app"
                    :components
                    ((:file "app")))
                   (:module "library"
                    :components
                    ((:file "library")))
                   (:module "lucerne"
                    :components
                    ((:file "lucerne"))))))))
  :description "A project skeleton generator."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op project-generator-test))))
