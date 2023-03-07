(uiop:define-package #:{{name}}-test
  (:use #:cl #:fiveam)
  (:export #:run-tests))
(in-package #:{{name}}-test)

(def-suite tests
  :description "{{name}} tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(defun run-tests ()
  (run! 'tests))
