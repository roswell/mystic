(in-package :cl-user)
(defpackage project-generator-test
  (:use :cl :fiveam)
  (:import-from :project-generator
                :validate-options
                :render)
  (:import-from :project-generator.skeleton.generic
                :+generic-template+))
(in-package :project-generator-test)

(def-suite project-generator)
(in-suite project-generator)

(test util
  (is
   (equal (project-generator.util:strip-whitespace "  test ")
          "test"))
  (is
   (equal (project-generator.util:parse-systems-list "a, b, c")
          (list "a" "b" "c"))))

(test options
  (signals project-generator:missing-required-option
    (validate-options +generic-template+
                      (list :name "my-project")))
  (finishes
    (validate-options +generic-template+
                      (list :name "my-project"
                            :author "me"
                            :license "MIT")))
  (let ((options (validate-options +generic-template+
                                   (list :name "my-project"
                                         :author "me"
                                         :license "MIT"
                                         :dependencies "a, b, c"))))
    (is
     (equal (getf options :name) "my-project"))
    (is
     (equal (getf options :author) "me"))
    (is
     (equal (getf options :dependencies)
            ":a
               :b
               :c"))))

(test render
  (let ((dir (asdf:system-relative-pathname :project-generator
                                            #p"my-project")))
    (finishes
      (render +generic-template+
              (list :name "my-project"
                    :author "me"
                    :license "MIT")
              dir))
    (is-true
     (probe-file (asdf:system-relative-pathname :project-generator
                                                #p"my-project/my-project.asd")))
    (is-true
     (probe-file (asdf:system-relative-pathname :project-generator
                                                #p"my-project/my-project-test.asd")))
    (is-true
     (probe-file (asdf:system-relative-pathname :project-generator
                                                #p"my-project/README.md")))
    (is-true
     (probe-file (asdf:system-relative-pathname :project-generator
                                                #p"my-project/src/my-project.lisp")))
    (is-true
     (probe-file (asdf:system-relative-pathname :project-generator
                                                #p"my-project/t/my-project.lisp")))
    (finishes
      (uiop:delete-directory-tree (uiop:ensure-directory-pathname dir) :validate t))))

(run! 'project-generator)
