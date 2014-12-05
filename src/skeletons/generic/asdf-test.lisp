(defsystem {{name}}-test
  :author "{{author}}"
  :license "{{license}}"
  :depends-on (:{{name}}
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "{{name}}")))))
