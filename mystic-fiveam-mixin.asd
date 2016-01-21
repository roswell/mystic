(defsystem mystic-fiveam-mixin
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:mystic
               :mystic-file-mixin)
  :components ((:module "templates/mixins/fiveam"
                :components
                ((:file "fiveam"))))
  :description "A Mystic mixin to add a FiveAM test system and test suite.")
