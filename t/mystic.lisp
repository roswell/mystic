(in-package :cl-user)
(defpackage mystic-test
  (:use :cl :fiveam)
  (:import-from :mystic
                :validate-options)
  (:export :run-tests))
(in-package :mystic-test)

(def-suite mystic
  :description "Mystic tests")
(in-suite mystic)

(test util
  (is
   (equal (mystic.util:strip-whitespace "  test ")
          "test"))
  (is
   (equal (mystic.util:parse-systems-list "a, b, c")
          (list "a" "b" "c"))))

(test render
  (let* ((dir (asdf:system-relative-pathname :mystic
                                             #p"t/my-project"))
         (true-dir (uiop:ensure-directory-pathname dir)))
    (finishes
      (mystic:render (make-instance 'mystic.template.library:library-template)
                     (list :name "my-project"
                           :author "me"
                           :license "MIT")
                     dir))
    (flet ((file (pathname)
             (asdf:system-relative-pathname :mystic
                                            (merge-pathnames pathname
                                                             #p"t/my-project/"))))
      ;; Test all the files exist
      (is-true
       (probe-file (file #p"my-project.asd")))
      (is-true
       (probe-file (file #p"my-project-test.asd")))
      (is-true
       (probe-file (file #p"README.md")))
      (is-true
       (probe-file (file #p".gitignore")))
      (is-true
       (probe-file (file #p".travis.yml")))
      (is-true
       (probe-file (file #p"src/my-project.lisp")))
      (is-true
       (probe-file (file #p"t/my-project.lisp")))
      ;; Test they contain what we want
      (flet ((has (string file)
               (is
                (integerp (search string (uiop:read-file-string (file file)))))))
        (dolist (str (list "defsystem my-project"
                           ":author \"me\""
                           ":maintainer \"me\""
                           ":license \"MIT\""
                           ":depends-on ()"))
          (has str #p"my-project.asd"))
        (has "defsystem my-project-test" #p"my-project-test.asd")
        (has "# my-project" #p"README.md")
        (has "defpackage my-project" #p"src/my-project.lisp")
        (has "defpackage my-project-test" #p"t/my-project.lisp")))
    (uiop:delete-directory-tree true-dir :validate t)))

(defun run-tests ()
  (run! 'mystic))
