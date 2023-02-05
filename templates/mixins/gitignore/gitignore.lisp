(uiop:define-package #:mystic.template.gitignore
  (:use #:cl)
  (:import-from #:mystic.util
                #:read-template-file
                #:render-string
                #:write-file)
  (:export #:gitignore-mixin)
  (:documentation "A Mystic mixin to add a .gitignore file to a project."))
(in-package #:mystic.template.gitignore)

;;; Classes

(defclass gitignore-mixin (mystic:template)
  ()
  (:documentation "A Mystic mixin to add a .gitignore file to a project."))

;;; Render

(defmethod mystic:render-template progn ((template gitignore-mixin) options directory)
  (declare (type list options)
           (type pathname directory))
  (let ((string (read-template-file #p"mixins/gitignore/gitignore.txt")))
    (write-file (render-string string options)
                (merge-pathnames #p".gitignore"
                                 directory))))
