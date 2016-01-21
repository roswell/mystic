# Mystic

[![Build Status](https://travis-ci.org/roswell/mystic.svg?branch=master)](https://travis-ci.org/roswell/mystic)

A modular project skeleton generator.

# Overview

Some features:

* **Declarative:** Options are specified in a declarative interface, so you have
  have multiple front-ends (command line, web, etc.) to the project generator.

* **Modular:** Most of the functionality common to different templates
  (e.g. Travis support, the `.gitignore` file) is implemented as mixins.

# Usage

```lisp
(mystic:render (make-instance 'mystic.template.library:library-template)
               (list :name "your-project"
                     :author "You"
                     :email "you@gmail.com"
                     :license "MIT"
                     :description "A one-line description of your project."
                     :dependencies "clack, postmodern")
               #p"/home/you/code/your-project/")
```

I'll get around to writing an actual interface at some point.

# License

Copyright (c) 2016 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
