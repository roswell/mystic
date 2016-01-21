(defsystem mystic-file-mixin
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:mystic)
  :components ((:module "templates/mixins/file"
                :components
                ((:file "file"))))
  :description "A Mystic mixin to render a list of files with Mustache.")
