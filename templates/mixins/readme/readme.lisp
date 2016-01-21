(in-package :cl-user)
(defpackage mystic.template.readme
  (:use :cl)
  (:import-from :mystic.util
                :read-template-file
                :render-string
                :write-file)
  (:export :readme-mixin)
  (:documentation "A Mystic mixin to add a README.md file."))
(in-package :mystic.template.readme)

;;; Classes

(defclass readme-mixin (mystic:template)
  ()
  (:documentation "A Mystic mixin to add a README.md file."))

;;; Render

(defmethod mystic:render-template progn ((template readme-mixin) options directory)
  (declare (type list options)
           (type pathname directory))
  (let ((readme (read-template-file #p"mixins/readme/source.md")))
    (write-file (render-string readme options)
                (merge-pathnames #p"README.md"
                                 directory))))
