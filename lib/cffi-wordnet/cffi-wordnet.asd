;; ASDF package description for cffi-wordnet              -*- Lisp -*-

(defpackage :cffi-wordnet-system (:use :cl :asdf))
(in-package :cffi-wordnet-system)

(defsystem cffi-wordnet
  :name "cffi-wordnet"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "CFFI Interface to Wordnet 3.0"
  :depends-on (:cffi
	       :cl-ppcre
               :graph-utils
               :alexandria)
  :components ((:file "wordnet-package")
	       (:file "cffi" :depends-on ("wordnet-package"))
	       (:file "constants" :depends-on ("cffi"))
	       (:file "wordnet" :depends-on ("constants"))))
