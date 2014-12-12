(in-package :cl-user)
(defpackage project-gen.skel.generic
  (:use :cl :project-gen.gen)
  (:import-from :project-gen.util
                :read-skeleton)
  (:export :+generic-template+))
(in-package :project-gen.skel.generic)

(defparameter +generic-template+
  (make-instance
   '<template>
   :name "Generic Project"
   :docstring "An empty, generic ASDF system."
   :options
   (list (make-instance '<option>
                        :name :name
                        :requiredp t
                        :docstring "The project's name.")
         (make-instance '<option>
                        :name :author
                        :requiredp t
                        :docstring "The project author's name.")
         (make-instance '<option>
                        :name :email
                        :docstring "The project author's email.")
         (make-instance '<option>
                        :name :homepage
                        :docstring "The project's homepage.")
         (make-instance '<option>
                        :name :license
                        :requiredp t
                        :docstring "The project's license string, e.g. 'MIT', 'GPLv3'.")
         (make-instance '<option>
                        :name :description
                        :docstring "A short, one-line description of the project.")
         (make-instance '<option>
                        :name :dependencies
                        :processor (lambda (deps)
                                     (format nil "~{:~A~^~%               ~}"
                                             (project-gen.util:parse-systems-list deps)))
                        :docstring "The project's dependent systems, as a comma-separated list, e.g: 'local-time, lucerne, crane'."))
   :files
   (list (make-instance '<file>
                        :path "{{name}}.asd"
                        :content (read-skeleton #p"generic/asdf.lisp"))
         (make-instance '<file>
                        :path "{{name}}-test.asd"
                        :content (read-skeleton #p"generic/asdf-test.lisp"))
         (make-instance '<file>
                        :path "src/{{name}}.lisp"
                        :content (read-skeleton #p"generic/source.lisp"))
         (make-instance '<file>
                        :path "t/{{name}}.lisp"
                        :content (read-skeleton #p"generic/test.lisp"))
         (make-instance '<file>
                        :path "README.md"
                        :content (read-skeleton #p"generic/README.md")))))
