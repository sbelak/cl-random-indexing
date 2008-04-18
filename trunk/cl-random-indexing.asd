;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :cl-random-indexing.system
  (:use :cl :asdf))

(in-package :cl-random-indexing.system)

(defsystem :cl-random-indexing
  :name "cl-random-indexing"
  :description "Random indexing library."
  :author "Simon Belak"
  :version "0.1"
  :licence "MIT"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "random-indexing")
               (:file "simple-ri")))
