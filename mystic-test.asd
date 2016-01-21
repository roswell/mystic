(defsystem mystic-test
  :author "Fernando Borretti"
  :license "MIT"
  :depends-on (:mystic
               :mystic-library-template
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "mystic")))))
