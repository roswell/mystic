(uiop:define-package #:mystic.util
  (:use #:cl)
  (:export #:read-template-file
           #:strip-whitespace
           #:parse-comma-separated-list
           #:parse-systems-list
           #:render-string
           #:write-file)
  (:documentation "Utilities for Mystic."))
(in-package #:mystic.util)

(defun read-template-file (pathname &key (asdf-system :mystic))
  "Read a pathname relative to the templates/ directory into a string."
  (assert (uiop:relative-pathname-p pathname))
  (uiop:read-file-string
   (merge-pathnames pathname
                    (asdf:system-relative-pathname asdf-system #p"templates/"))))

(defun strip-whitespace (string)
  (string-trim '(#\Space #\Tab) string))

(defun parse-comma-separated-list (systems-list)
  (loop for system-name
        in (split-sequence:split-sequence #\, systems-list)
        collecting
        (strip-whitespace system-name)))

(defun parse-systems-list (systems-list)
  ;; TODO: leaved for backward compatibility
  (parse-comma-separated-list systems-list))

(defun render-string (template-string data)
  "Render a Mustache template string to a string."
  (with-output-to-string (str)
    (mustache:render
     template-string
     (loop for sublist on data by #'cddr collecting
       (cons (first sublist) (second sublist)))
     str)))

(defun write-file (string pathname)
  "Write a string to a file (absolute path), creating it if necessary, and
creating its parent directories as well."
  (assert (uiop:absolute-pathname-p pathname))
  (ensure-directories-exist (uiop:pathname-directory-pathname pathname))
  (with-open-file (output-stream pathname
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
    (write-string string output-stream))
  pathname)
