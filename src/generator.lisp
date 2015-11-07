(in-package :cl-user)
(defpackage project-generator
  (:use :cl :trivial-types :anaphora)
  ;; Classes and accessors
  (:export :file
           :file-path
           :file-content
           :option
           :option-name
           :option-docstring
           :option-required-p
           :option-default
           :template
           :template-name
           :template-options
           :template-files
           :missing-required-option)
  ;; Interface
  (:export :validate-options
           :render
           :list-templates
           :register-template))
(in-package :project-generator)

(defclass file ()
  ((path :reader file-path
         :initarg :path
         :type string
         :documentation "The path to the file, a Mustache template string.")
   (content :reader file-content
            :initarg :content
            :type string
            :documentation "The file's contents, a Mustache template string."))
  (:documentation "A file."))

(defclass option ()
  ((name :reader option-name
         :initarg :name
         :type keyword
         :documentation "The name of the option.")
   (docstring :reader option-docstring
              :initarg :docstring
              :type string
              :documentation "The option's documentation string.")
   (requiredp :reader option-required-p
              :initarg :requiredp
              :initform nil
              :type boolean
              :documentation "Whether the option is required.")
   (processor :reader option-processor
              :initarg :processor
              :initform (lambda (x) x)
              :type function
              :documentation "A function used to process the value given to the option.")
   (default :reader option-default
            :initarg
            :default
            :documentation "The option's default value."))
  (:documentation "An option to a template."))

(defclass template ()
  ((name :reader template-name
         :initarg :name
         :type string
         :documentation "The template's descriptive name.")
   (docstring :reader template-docstring
              :initarg :docstring
              :type string
              :documentation "The template's documentation string.")
   (options :reader template-options
            :initarg :options
            :type (proper-list option)
            :documentation "A list of template options.")
   (files :reader template-files
          :initarg :files
          :type (proper-list files)
          :documentation "A list of files to template."))
  (:documentation "Represents a template."))

(define-condition missing-required-option (simple-error)
  ((option-name :reader option-name
                :initarg :option-name
                :type keyword
                :documentation "The name of the required option."))
  (:report
   (lambda (condition stream)
     (format stream "The option '~A' was required but was not supplied."
             (option-name condition)))))

(defmethod validate-options ((template template) (options list))
  "Take a plist of options, and validate it against the options in the template."
  (let ((final-options (list)))
    (loop for option in (template-options template) do
      (aif (getf options (option-name option))
           ;; Was the option supplied? If so, apply the option's processor to it
           ;; and add it to the `final-options` list
           (setf (getf final-options (option-name option))
                 (funcall (option-processor option) it))
           ;; Otherwise, check if it has a default value
           (if (slot-boundp option 'default)
               ;; Use this value
               (setf (getf final-options (option-name option))
                     (option-default option))
               ;; No default? If the option is required, signal an error
               (if (option-required-p option)
                   (error 'missing-required-option
                          :option-name (option-name option))))))
    final-options))

(defun render-template (template-string data)
  (with-output-to-string (str)
    (mustache:render
     template-string
     (loop for sublist on data by #'cddr collecting
       (cons (first sublist) (second sublist)))
     str)))

(defmethod render ((template template) (options list) (directory pathname))
  (let ((options (validate-options template options))
        (directory (uiop:ensure-directory-pathname directory)))
    (setf (getf options :year)
          (write-to-string (local-time:timestamp-year (local-time:now))))
    (loop for file in (template-files template) do
      (let* ((file-path (parse-namestring (render-template (file-path file) options)))
             (full-file-path (merge-pathnames file-path
                                              directory))
             (content (render-template (file-content file) options)))
        (ensure-directories-exist
         (uiop:pathname-directory-pathname full-file-path))
        (with-open-file (output-stream full-file-path
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create)
          (write-string content output-stream))))))

(defvar *templates* (list)
  "The list of templates.")

(defun list-templates ()
  "Return a list of project templates."
  *templates*)

(defun register-template (template)
  "Register a template."
  (declare (type template template))
  (push template *templates*)
  template)
