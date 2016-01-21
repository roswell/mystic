(defsystem {{name}}-test
  :author "{{#email}}{{author}} <{{email}}>{{/email}}{{^email}}{{author}}{{/email}}"
  :license "{{license}}"
  :depends-on (:{{name}}
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "{{name}}")))))
