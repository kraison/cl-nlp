;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem :porter-stemmer
  :components ((:file "package")
	       (:file "stemmer" :depends-on ("package"))))
