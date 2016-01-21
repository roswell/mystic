(in-package :cl-user)
(defpackage mystic.template.library
  (:use :cl)
  (:import-from :mystic.util
                :read-template-file
                :parse-systems-list)
  (:import-from :mystic
                :prompt-option)
  (:import-from :mystic.template.file
                :file)
  (:export :library-template)
  (:documentation "A Mystic mixin to add a .travis.yml file."))
(in-package :mystic.template.library)

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
   (list (make-instance 'prompt-option
                        :name :name
                        :title "Name"
                        :requiredp t
                        :docstring "The project's name.")
         (make-instance 'prompt-option
                        :name :author
                        :title "Author"
                        :requiredp t
                        :docstring "The project author's name.")
         (make-instance 'prompt-option
                        :name :email
                        :title "Email"
                        :docstring "The project author's email.")
         (make-instance 'prompt-option
                        :name :homepage
                        :title "Homepage"
                        :docstring "The project's homepage.")
         (make-instance 'prompt-option
                        :name :license
                        :title "License"
                        :requiredp t
                        :docstring "The project's license string, e.g. 'MIT', 'GPLv3'.")
         (make-instance 'prompt-option
                        :name :description
                        :title "Description"
                        :docstring "A short, one-line description of the project.")
         (make-instance 'prompt-option
                        :name :dependencies
                        :title "Dependencies"
                        :processor (lambda (deps)
                                     (format nil "~{:~A~^~%               ~}"
                                             (parse-systems-list deps)))
                        :docstring "The project's dependent systems, as a comma-separated list, e.g: 'local-time, lucerne, crane'."))
   :files
   (list
    (make-instance 'file
                   :path "{{name}}.asd"
                   :content (read-template-file #p"library/asdf.lisp"))
    (make-instance 'file
                   :path "src/{{name}}.lisp"
                   :content (read-template-file #p"library/source.lisp"))))
  (:documentation "A Mystic mixin to add a .travis.yml file."))

(mystic:register-template (make-instance 'library-template))
