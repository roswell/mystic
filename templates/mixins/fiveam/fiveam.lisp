(in-package :cl-user)
(defpackage mystic.template.fiveam
  (:use :cl)
  (:import-from :mystic.util
                :read-template-file
                :render-string
                :write-file)
  (:export :fiveam-mixin)
  (:documentation "A Mystic mixin to add a FiveAM test system and test suite."))
(in-package :mystic.template.fiveam)

;;; Classes

(defclass fiveam-mixin (mystic:template)
  ()
  (:documentation "A Mystic mixin to add a FiveAM test system and test suite."))

;;; Render

(defmethod mystic:render-template progn ((template fiveam-mixin) options directory)
  (declare (type list options)
           (type pathname directory))
  (let ((system (read-template-file #p"mixins/fiveam/system.lisp"))
        (source (read-template-file #p"mixins/fiveam/code.lisp")))
    (write-file (render-string system options)
                (merge-pathnames (parse-namestring
                                  (render-string "{{name}}-test.asd" options))
                                 directory))
    (write-file (render-string source options)
                (merge-pathnames (parse-namestring
                                  (render-string "t/{{name}}.lisp" options))
                                 directory))))
