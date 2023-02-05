(uiop:define-package #:mystic.template.travis
  (:use #:cl)
  (:import-from #:mystic.util
                #:read-template-file
                #:render-string
                #:write-file)
  (:export #:travis-mixin)
  (:documentation "A Mystic mixin to add a .travis.yml file to a project."))
(in-package #:mystic.template.travis)

;;; Classes

(defclass travis-mixin (mystic:template)
  ()
  (:documentation "A Mystic mixin to add a .travis.yml file to a project."))

;;; Render

(defmethod mystic:render-template progn ((template travis-mixin) options directory)
  (declare (type list options)
           (type pathname directory))
  (let ((string (read-template-file #p"mixins/travis/travis.yaml")))
    (write-file (render-string string options)
                (merge-pathnames #p".travis.yml"
                                 directory))))
