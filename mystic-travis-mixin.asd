(defsystem mystic-travis-mixin
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:mystic
               :mystic-file-mixin)
  :components ((:module "templates/mixins/travis"
                :components
                ((:file "travis"))))
  :description "A Mystic mixin to add a .travis.yml file.")
