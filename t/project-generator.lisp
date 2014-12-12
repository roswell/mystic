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
                                            :author "me")))
  (let ((options
          (project-gen.gen:validate-options project-gen.skel.generic:+generic-template+
                                            (list :name "my-project"
                                                  :author "me"
                                                  :dependencies "a, b, c"))))
    (is
     (equal (getf options :name) "my-project"))
    (is
     (equal (getf options :author) "me"))
    (is
     (equal (getf options :dependencies) (list "a" "b" "c")))
    (is (equal (length options) 6))))

(run! 'project-generator)
