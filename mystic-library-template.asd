(defsystem mystic-library-template
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:mystic
               :mystic-readme-mixin
               :mystic-gitignore-mixin
               :mystic-fiveam-mixin
               :mystic-travis-mixin)
  :components ((:module "templates/library"
                :components
                ((:file "library"))))
  :description "A Mystic mixin to add a .travis.yml file.")
