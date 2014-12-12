(defsystem project-generator-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:project-generator
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "project-generator")))))
