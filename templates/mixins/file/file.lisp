(in-package :cl-user)
(defpackage mystic.template.file
  (:use :cl)
  (:import-from :mystic.util
                :render-string
                :write-file)
  (:export :file
           :file-path
           :file-content
           :file-mixin)
  (:documentation "A Mystic template mixin for rendering a list of files using
 Mustache."))
(in-package :mystic.template.file)

;;; Classes

(defclass file ()
  ((path :reader file-path
         :initarg :path
         :type string
         :documentation "The path to the file relative to the directory,
a Mustache template string.")
   (content :reader file-content
            :initarg :content
            :type string
            :documentation "The file's contents, a Mustache template string."))
  (:documentation "A file."))

(defclass file-mixin (mystic:template)
  ((files :reader template-files
          :initarg :files
          :type list
          :documentation "A list of files to template."))
  (:documentation "A Mystic template mixin for rendering a list of files using Mustache."))

;;; Render

(defmethod mystic:render-template progn ((template file-mixin) options directory)
  (declare (type list options)
           (type pathname directory))
  (loop for file in (template-files template) do
    (let* ((file-path (parse-namestring (render-string (file-path file) options)))
           (full-file-path (merge-pathnames file-path
                                            directory))
           (content (mystic.util:render-string (file-content file) options)))
      (write-file content full-file-path))))
