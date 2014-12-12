(in-package :cl-user)
(defpackage project-gen-test
  (:use :cl :fiveam))
(in-package :project-gen-test)

(def-suite project-generator)
(in-suite project-generator)

(test util
  (is
   (equal (project-gen.util:strip-whitespace "  test ")
          "test"))
  (is
   (equal (project-gen.util:parse-systems-list "a, b, c")
          (list "a" "b" "c"))))

(test options
  (signals project-gen.gen:<missing-required-option>
   (project-gen.gen:validate-options project-gen.skel.generic:+generic-template+
                                     (list :name "my-project")))
  (finishes
    (project-gen.gen:validate-options project-gen.skel.generic:+generic-template+
                                      (list :name "my-project"
                                            :author "me"
                                            :license "MIT")))
  (let ((options
          (project-gen.gen:validate-options project-gen.skel.generic:+generic-template+
                                            (list :name "my-project"
                                                  :author "me"
                                                  :license "MIT"
                                                  :dependencies "a, b, c"))))
    (is
     (equal (getf options :name) "my-project"))
    (is
     (equal (getf options :author) "me"))
    (is
     (equal (getf options :dependencies) (list "a" "b" "c")))))

(test render
  (let ((dir (asdf:system-relative-pathname :project-generator
                                            #p"my-project")))
    (finishes
      (project-gen.gen:render project-gen.skel.generic:+GENERIC-TEMPLATE+
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
      (fad:delete-directory-and-files dir))))

(run! 'project-generator)
