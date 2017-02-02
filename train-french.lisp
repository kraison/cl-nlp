(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push #P"./" asdf:*central-registry*)
(push #P"../porter-stemmer/" asdf:*central-registry*)
(push #P"../graph-utils/" asdf:*central-registry*)
(push #P"../cffi-wordnet/" asdf:*central-registry*)

(ql:quickload :nlp)

(in-package :nlp)

(make-french-db :save-p t)
(close-language (lookup-language "french"))

(quit 0)
