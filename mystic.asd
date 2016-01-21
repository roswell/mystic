(defsystem mystic
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:cl-mustache
               :split-sequence
               :anaphora
               :local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:file "util")
                 (:file "mystic"))))
  :description "A project skeleton generator."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op mystic-test))))
