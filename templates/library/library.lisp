(uiop:define-package #:mystic.template.library
  (:use #:cl)
  (:import-from #:mystic.util
                #:read-template-file
                #:parse-comma-separated-list)
  (:import-from #:mystic
                #:make-option)
  (:import-from #:mystic.template.file
                #:make-file)
  (:export #:library-template)
  (:documentation "A Mystic mixin to add a .travis.yml file."))
(in-package #:mystic.template.library)

;;; Classes

(defclass library-template (mystic.template.file:file-mixin
                            mystic.template.readme:readme-mixin
                            mystic.template.gitignore:gitignore-mixin
                            mystic.template.fiveam:fiveam-mixin
                            mystic.template.travis:travis-mixin)
  ()
  (:default-initargs
   :name "Library"
   :docstring "An empty Common Lisp library."
   :options
   (list (make-option :name
                      "Name"
                      "The project's name."
                      :requiredp t)
         (make-option :author
                      "Author"
                      "The project author's name."
                      :requiredp t)
         (make-option :email
                      "Email"
                      "The project author's email.")
         (make-option :homepage
                      "Homepage"
                      "The project's homepage.")
         (make-option :license
                      "License"
                      "The project's license string, e.g. 'MIT', 'GPLv3'."
                      :requiredp t)
         (make-option :description
                      "Description"
                      "A short, one-line description of the project.")
         (make-option :dependencies
                      "Dependencies"
                      "The project's dependent systems, as a comma-separated list, e.g: 'local-time, lucerne, crane'."
                      :processor (lambda (deps)
                                   (format nil "~{:~A~^~%               ~}"
                                           (parse-comma-separated-list deps)))))
   :files
   (list
    (make-file :mystic
               "library/asdf.lisp"
               "{{name}}.asd")
    (make-file :mystic
               "library/source.lisp"
               "src/{{name}}.lisp")))
  (:documentation "A Mystic class to create empty Common Lisp library."))

(mystic:register-template (make-instance 'library-template))
