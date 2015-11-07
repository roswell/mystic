(in-package :cl-user)
(defpackage project-generator.skeleton.generic
  (:use :cl)
  (:import-from :project-generator
                :template
                :option
                :file)
  (:import-from :project-generator.util
                :read-skeleton
                :parse-systems-list)
  (:export :+generic-template+))
(in-package :project-generator.skeleton.generic)

(defparameter +generic-template+
  (make-instance
   'template
   :name "Generic Project"
   :docstring "An empty, generic ASDF system."
   :options
   (list (make-instance 'option
                        :name :name
                        :requiredp t
                        :docstring "The project's name.")
         (make-instance 'option
                        :name :author
                        :requiredp t
                        :docstring "The project author's name.")
         (make-instance 'option
                        :name :email
                        :docstring "The project author's email.")
         (make-instance 'option
                        :name :homepage
                        :docstring "The project's homepage.")
         (make-instance 'option
                        :name :license
                        :requiredp t
                        :docstring "The project's license string, e.g. 'MIT', 'GPLv3'.")
         (make-instance 'option
                        :name :description
                        :docstring "A short, one-line description of the project.")
         (make-instance 'option
                        :name :dependencies
                        :processor (lambda (deps)
                                     (format nil "~{:~A~^~%               ~}"
                                             (parse-systems-list deps)))
                        :docstring "The project's dependent systems, as a comma-separated list, e.g: 'local-time, lucerne, crane'."))
   :files
   (list (make-instance 'file
                        :path "{{name}}.asd"
                        :content (read-skeleton #p"generic/asdf.lisp"))
         (make-instance 'file
                        :path "{{name}}-test.asd"
                        :content (read-skeleton #p"generic/asdf-test.lisp"))
         (make-instance 'file
                        :path "src/{{name}}.lisp"
                        :content (read-skeleton #p"generic/source.lisp"))
         (make-instance 'file
                        :path "t/{{name}}.lisp"
                        :content (read-skeleton #p"generic/test.lisp"))
         (make-instance 'file
                        :path "README.md"
                        :content (read-skeleton #p"generic/README.md"))
         (make-instance 'file
                        :path ".gitignore"
                        :content (read-skeleton #p"generic/gitignore.txt"))
         (make-instance 'file
                        :path ".travis.yml"
                        :content (read-skeleton #p"generic/travis.yml")))))
