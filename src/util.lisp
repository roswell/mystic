(in-package :cl-user)
(defpackage project-generator.util
  (:use :cl)
  (:export :read-skeleton
           :strip-whitespace
           :parse-systems-list))
(in-package :project-generator.util)

(defun read-skeleton (path)
  (uiop:read-file-string
   (merge-pathnames
    path
    (asdf:system-relative-pathname :project-generator
                                   #p"src/skeletons/"))))

(defun strip-whitespace (string)
  (string-trim '(#\Space #\Tab) string))

(defun parse-systems-list (systems-list)
  (loop for system-name
        in (split-sequence:split-sequence #\, systems-list)
        collecting
        (strip-whitespace system-name)))
