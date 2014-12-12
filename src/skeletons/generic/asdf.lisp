(defsystem {{name}}
  :version "0.1"
  :author "{{#email}}{{author}} <{{email}}>{{/email}}{{^email}}{{author}}{{/email}}"
  :maintainer "{{#email}}{{author}} <{{email}}>{{/email}}{{^email}}{{author}}{{/email}}"
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
