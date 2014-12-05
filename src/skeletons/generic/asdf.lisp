(defsystem {{name}}
  :version "{{version}}"
  :author "{{author}} <{{email}}>"
  :maintainer "{{author}} <{{email}}>"
  :license "{{license}}"
  :depends-on ({{deps}})
  :components ((:module "src"
                :serial t
                :components
                ((:file "{{name}}"))))
  :description "{{desc}}"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op {{name}}-test))))
