(in-package :cl-user)
(defpackage project-gen.gen
  (:use :cl :trivial-types)
  (:export :<file>
           :path
           :content
           :<option>
           :name
           :docstring
           :requiredp
           :default
           :<template>
           :name
           :options
           :files
           :read-skeleton))
(in-package :project-gen.gen)

(defclass <file> ()
  ((path :reader path
         :initarg :path
         :type string
         :documentation "The path to the file, a Mustache template string.")
   (content :reader content
            :initarg :content
            :type string
            :documentation "The file's contents, a mustache template string."))
  (:documentation "A file."))

(defclass <option> ()
  ((name :reader name
         :initarg :name
         :type keyword
         :documentation "The name of the option.")
   (docstring :reader docstring
              :initarg :docstring
              :type string
              :documentation "The option's documentation string.")
   (requiredp :reader requiredp
              :initarg :requiredp
              :initform nil
              :type boolean
              :documentation "Whether the option is required.")
   (default :reader default
            :initarg :default
            :documentation "The option's default value."))
  (:documentation "An option to a template."))

(defclass <template> ()
  ((name :reader name
         :initarg :name
         :type string
         :documentation "The template's descriptive name.")
   (docstring :reader docstring
              :initarg :docstring
              :type string
              :documentation "The template's documentation string.")
   (options :reader options
            :initarg :options
            :type (proper-list <option>)
            :documentation "A list of template options.")
   (files :reader files
          :initarg :files
          :type (proper-list files)
          :documentation "A list of files to template."))
  (:documentation "Represents a template."))

(defun read-skeleton (path)
  (uiop:read-file-string
   (merge-pathnames
    path
    (asdf:system-relative-pathname :project-generator
                                   #p"src/skeletons/"))))

(defun strip-whitespace (string)
  (string-trim '(#\Space #\Tab) string))

(defun parse-systems-list (systems-list)
  (loop for system-name
        in (split-sequence:split-sequence #\, systems-list)
        collecting
        (strip-whitespace system-name)))

(defmethod render ((template <template>) (options list) (pathname pathname))
  )
