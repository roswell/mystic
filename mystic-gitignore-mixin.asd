(defsystem mystic-gitignore-mixin
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:mystic
               :mystic-file-mixin)
  :components ((:module "templates/mixins/gitignore"
                :components
                ((:file "gitignore"))))
  :description "A Mystic mixin to add a .gitignore file.")
