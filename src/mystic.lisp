(uiop:define-package #:mystic
  (:use #:cl)
  (:import-from #:anaphora
                #:aif
                #:it)
  (:export #:option
           #:option-title
           #:option-name
           #:option-docstring
           #:option-required-p
           #:option-default)
  (:export #:prompt-option
           #:multiple-choice-option
           #:boolean-option
           #:make-option)
  (:export #:template
           #:template-name
           #:template-docstring
           #:template-options)
  (:export #:missing-required-option
           #:bad-option-value
           #:ask-about-option-without-default
           #:ask-about-value-from-config
           #:ask-about-defaults
           #:base-option-condition)
  (:export #:validate-options
           #:render-template
           #:render
           #:list-templates
           #:register-template))
(in-package #:mystic)

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
            :initform nil
            :documentation "The option's default value. This value will not be passed through an option processor."))
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

(define-condition base-option-condition ()
  ((option-name :reader option-name
                :initarg :option-name
                :type keyword
                :documentation "The name of the option.")
   (docstring :reader option-docstring
              :initarg :docstring
              :type string
              :documentation "Documentation of the option.")))


(define-condition ask-about-defaults (base-option-condition)
  ((option-default :reader option-default
                   :initarg :option-default
                   :documentation "The default value of the option."))
  (:report
   (lambda (condition stream)
     (format stream "Value for option '~A' was not supplied.~@
                     ~@
                     This option should be ~S~@
                     ~@
                     Do you want to enter the value or to use default: ~S?"
             (option-name condition)
             (option-docstring condition)
             (option-default condition))))
  (:documentation "Signalled when user passed REQUEST-ALL-OPTIONS-P argument to RENDER method and didn't supply some option value which has a default."))


(define-condition ask-about-value-from-config (base-option-condition)
  ((value :reader option-value
          :initarg :value
          :documentation "The value from config.")
   (config-path :reader config-path
                :initarg :config-path
                :documentation "Path to the config."))
  (:report
   (lambda (condition stream)
     (format stream "Value for option '~A' was not supplied, but was found in config:~@
                     ~A~@
                     ~@
                     This option should be ~S~@
                     ~@
                     Do you want to enter another value or to use value from config: ~S?"
             (option-name condition)
             (config-path condition)
             (option-docstring condition)
             (option-value condition))))
  (:documentation "Signalled when user passed REQUEST-ALL-OPTIONS-P argument to RENDER method, didn't supply some option value but a value for this option was found in a config file."))


(define-condition ask-about-option-without-default (base-option-condition)
  ((value :reader option-value
          :initarg :value
          :documentation "The value from config."))
  (:report
   (lambda (condition stream)
     (format stream "Value for option '~A' was not supplied, and it has no default.~@
                     ~@
                     This option should be ~S~@
                     ~@
                     Do you want to enter another value or leave this option not filled?"
             (option-name condition)
             (option-docstring condition))))
  (:documentation "Signalled when user passed REQUEST-ALL-OPTIONS-P argument to RENDER method, didn't supply an option value and we don't have a default or saved value for it."))


(define-condition missing-required-option (base-option-condition)
  ()
  (:report
   (lambda (condition stream)
     (format stream "The option '~A' is required but was not supplied.~@
                     ~@
                     This option should be ~S."
             (option-name condition)
             (option-docstring condition))))
  (:documentation "Signalled when a required option is not supplied."))


(define-condition bad-option-value (base-option-condition)
  ((value :reader option-value
          :initarg :value
          :initform (error "Argument :value is required.")
          :documentation "Wrong value of an option.")
   (problem :reader option-value-problem
            :initarg :problem
            :initform (error "Argument :problem is required.")
            :documentation "What is wrong with value."))
  (:report
   (lambda (condition stream)
     (format stream "Invalid value ~S was given for option '~A'.~@
                     ~@
                     ~A"
             (option-value condition)
             (option-name condition)
             (option-value-problem condition))))
  (:documentation "Signalled when a some value is in wrong format or type."))

;;; Methods

(defun read-new-value ()
  (format *query-io* "~&Enter a new value: ")
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))


(defun config-pathname ()
  (ubiquitous:designator-pathname ubiquitous:*storage-pathname*
                                  ubiquitous:*storage-type*))


(defgeneric validate-options (template options &key request-all-options-p)
  (:documentation "Validates templates and returns a plist where keys are option names. This plist will be used to render templates."

                  "A template or mixin might define a method for this generic function either to
                   apply some additional validation or to add calculated variables to the plist. For example,
                   if some option contains a system name, you might want to uppercase it or to replace
                   dashes with underscores and store results to a separate variable."))


(defmethod validate-options ((template template) (options list) &key request-all-options-p)
  "Take a plist of options, and validate it against the options in the template.

   When ASK-ABOUT-DEFAULTS-P is T, Mystic will ask user about all options missing from OPTIONS list
   but used in TEMPLATE. Asking is performed as condition signaling and a user can choose one of
   available restarts:

   - USE-DEFAULT - use default specified in the option's code.
   - USE-VALUE-FROM-CONFIG - use value from config.
   - USE-VALUE - enter a value interactively.
   - STORE-VALUE - enter a value and store it in the config for future calls.
"
  (ubiquitous:with-local-storage ('options)
    (loop with config-path = (config-pathname)
          with all-options = (template-options template)
          for option in all-options
          for name = (option-name option)
          for docstring = (option-docstring option)
          for value = (getf options (option-name option))
          for default = (option-default option)
          for required = (option-required-p option)
          for value-from-config = (ubiquitous:value name)
          append (list name
                       (restart-case
                           (cond
                             ;; Was the option supplied? If so, apply the option's processor to it
                             ;; and add it to the `final-options` list
                             (value
                              (funcall (option-processor option)
                                       value))
                             (value-from-config
                              (if request-all-options-p
                                  (error 'ask-about-value-from-config
                                         :option-name name
                                         :docstring docstring
                                         :value value-from-config
                                         :config-path config-path)
                                  value-from-config))
                             ;; Otherwise, check if it has a default value?
                             ;; Then use this value:
                             (default (if request-all-options-p
                                          (error 'ask-about-defaults
                                                 :option-name name
                                                 :docstring docstring
                                                 :option-default default)
                                          default))
                             ;; No default? If the option is required, signal an error
                             (required
                              (error 'missing-required-option
                                     :option-name name
                                     :docstring docstring))
                             ;; When option is not required and there is no default,
                             ;; but user wants to decide about all options:
                             (request-all-options-p
                              (error 'ask-about-option-without-default
                                     :option-name name
                                     :docstring docstring)))
                         (continue ()
                           :report "Leave this option unfilled."
                           :test (lambda (condition)
                                   (declare (ignore condition))
                                   ;; Required options always should be filled, because
                                   ;; otherwise template might not render propertly.
                                   ;; That is why we don't provide this restart for required options.
                                   (unless required
                                     t))
                           nil)
                         (use-default ()
                           :report (lambda (stream)
                                     (format stream "Use default value ~S."
                                             default))
                           :test (lambda (condition)
                                   (declare (ignore condition))
                                   (not (null default)))
                           default)
                         (use-value-from-config ()
                           :report (lambda (stream)
                                     (format stream "Use value from config ~S (~A)."
                                             value-from-config
                                             config-path))
                           :test (lambda (condition)
                                   (declare (ignore condition))
                                   (not (null value-from-config)))
                           value-from-config)
                         (use-value (option-value)
                           :report "Provide a value."
                           :interactive read-new-value
                           option-value)
                         (store-value (option-value)
                           :report (lambda (stream)
                                     (format stream "Provide a value and save it for future renders to ~A."
                                             config-path))
                           :interactive read-new-value
                           (setf (ubiquitous:value name)
                                 option-value)
                           option-value))))))

(defgeneric render-template (template options directory)
  (:documentation "Render a @cl:param(template) to a @cl:param(directory). The
@cl:param(options) are a plist of option names to their supplied values.")

  (:method-combination progn))

(defun render (template options directory &key request-all-options-p)
  "Render a @cl:param(template) to a @cl:param(directory). The
@cl:param(options) are a plist of option names to their supplied values.

This is the user-level entrypoint, and performs option validation and anything
that needs to be done before handing over the task of rendering to the actual
@c(render-template) method."
  (declare (type list options)
           (type pathname directory))
  (let ((options (validate-options template options
                                   :request-all-options-p request-all-options-p))
        (directory (uiop:ensure-directory-pathname directory)))
    (setf (getf options :year)
          (write-to-string (local-time:timestamp-year (local-time:now))))
    (ensure-directories-exist directory)
    (render-template template options directory)
    (values)))

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


(defun make-option (name title docstring &key requiredp (processor #'identity) default)
  (make-instance 'prompt-option
                 :name name
                 :title title
                 :requiredp requiredp
                 :docstring docstring
                 :processor processor
                 :default default))


(defmethod print-object ((obj prompt-option) stream)
  (print-unreadable-object (obj stream :type t)
    (loop with first-item-p = t
          for slot in '(name requiredp default docstring)
          for value = (when (slot-boundp obj slot)
                        (slot-value obj slot))
          when value
            do (if first-item-p
                   (setf first-item-p nil)
                   (write-char #\Space stream))
               (format stream "~S ~A"
                       (alexandria:make-keyword slot)
                       value))))
