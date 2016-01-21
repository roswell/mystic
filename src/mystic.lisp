(in-package :cl-user)
(defpackage mystic
  (:use :cl)
  (:import-from :anaphora
	        :aif
                :it)
  (:export :option
           :option-title
           :option-name
           :option-docstring
           :option-required-p
           :option-default)
  (:export :prompt-option
           :multiple-choice-option
           :boolean-option)
  (:export :template
           :template-name
           :template-docstring
           :template-options)
  (:export :missing-required-option)
  (:export :validate-options
           :render-template
           :render
           :list-templates
           :register-template))
(in-package :mystic)

;;; Classes

(defclass option ()
  ((name :reader option-name
         :initarg :name
         :type keyword
         :documentation "The name of the option, a keyword, e.g. @c(:project-name).")
   (title :reader option-title
          :initarg :title
          :type string
          :documentation "The human-readable option name, e.g. @c(\"Project Name\").")
   (docstring :reader option-docstring
              :initarg :docstring
              :type string
              :documentation "The option's documentation string.")
   (requiredp :reader option-required-p
              :initarg :requiredp
              :initform nil
              :type boolean
              :documentation "Whether the option is required. False by default.")
   (processor :reader option-processor
              :initarg :processor
              :initform #'identity
              :type function
              :documentation "A function used to process the value given to the option.")
   (default :reader option-default
            :initarg :default
            :documentation "The option's default value."))
  (:documentation "An option to a template."))

(defclass prompt-option (option)
  ()
  (:documentation "An option that prompts the user for a value."))

(defclass multiple-choice-option (option)
  ((choices :reader option-choices
            :initarg :choices
            :type list
            :documentation "A list of strings."))
  (:documentation "An option that gives the user a choice from a list."))

(defclass boolean-option (option)
  ()
  (:documentation "A yes or no option."))

(defclass template ()
  ((name :reader template-name
         :initarg :name
         :type string
         :documentation "The template's human-readable name.")
   (docstring :reader template-docstring
              :initarg :docstring
              :type string
              :documentation "The template's documentation string.")
   (options :reader template-options
            :initarg :options
            :type list
            :documentation "A list of template options."))
  (:documentation "Represents a template."))

;;; Errors

(define-condition missing-required-option (simple-error)
  ((option-name :reader option-name
                :initarg :option-name
                :type keyword
                :documentation "The name of the required option."))
  (:report
   (lambda (condition stream)
     (format stream "The option '~A' was required but was not supplied."
             (option-name condition))))
  (:documentation "Signalled when a required option is not supplied."))

;;; Methods

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

(defgeneric render-template (template options directory)
  (:documentation "Render a @cl:param(template) to a @cl:param(directory). The
@cl:param(options) are a plist of option names to their supplied values.")

  (:method-combination progn))

(defun render (template options directory)
  "Render a @cl:param(template) to a @cl:param(directory). The
@cl:param(options) are a plist of option names to their supplied values.

This is the user-level entrypoint, and performs option validation and anything
that needs to be done before handing over the task of rendering to the actual
@c(render-template) method."
  (declare (type list options)
           (type pathname directory))
  (let ((options (validate-options template options))
        (directory (uiop:ensure-directory-pathname directory)))
    (setf (getf options :year)
          (write-to-string (local-time:timestamp-year (local-time:now))))
    (ensure-directories-exist directory)
    (render-template template options directory)))

(defvar *templates* (list)
  "The list of templates.")

(defun list-templates ()
  "Return a list of project templates."
  *templates*)

(defun register-template (template)
  "Register a template."
  (declare (type template template))
  (pushnew template *templates* :test #'(lambda (a b)
                                          (eq (class-name (class-of a))
                                              (class-name (class-of b)))))
  template)
