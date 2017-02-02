;; ASDF package description for nlp              -*- Lisp -*-

(defpackage :nlp-system (:use :cl :asdf))
(in-package :nlp-system)

(defsystem nlp
  :name "cl-nlp"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "2.0"
  :description "Multi-language Natural Language Processing Utilities"
  :long-description "Multi-language Natural Language Processing Utilities."
  :depends-on (:cl-ppcre
               :babel
               :cl-kyoto-cabinet
               :cffi
               :cl-store
               :parse-number
               :dso-lex
               :yacc
               :alexandria
               :porter-stemmer
               #+sbcl :sb-concurrency
               :cl-heap
               :rcl
               :log4cl
               :graph-utils
               :cffi-wordnet
               :cl-fad
               :s-xml
               :cxml
               :split-sequence)
  :components ((:file "nlp-package")
               (:file "globals" :depends-on ("nlp-package"))
               (:file "pos-symbols" :depends-on ("globals"))
               (:file "utilities" :depends-on ("pos-symbols"))
               (:file "db" :depends-on ("utilities"))
               (:file "kyoto-lexicon" :depends-on ("db"))
               (:file "db-methods" :depends-on ("kyoto-lexicon"))
               (:file "corpora" :depends-on ("utilities"))
               (:file "pos-map" :depends-on ("corpora"))

               (:file "read-cess-esp" :depends-on ("corpora"))
               (:file "read-ancora-es" :depends-on ("corpora"))
               (:file "read-wikicorpus-es" :depends-on ("corpora"))
               (:file "read-tut" :depends-on ("corpora"))
               (:file "read-paisa" :depends-on ("corpora"))
               (:file "read-floresta" :depends-on ("corpora"))
               (:file "read-tiger" :depends-on ("corpora"))

               (:file "base-forms" :depends-on ("globals"))
               (:file "lexicon" :depends-on ("kyoto-lexicon"))
               (:file "stop-words" :depends-on ("db-methods"))
               (:file "sentence-splitter" :depends-on ("utilities"))
               (:file "libstemmer" :depends-on ("db-methods"))
               (:file "stemmer" :depends-on ("libstemmer"))
               (:file "edit-distance" :depends-on ("utilities"))
               (:file "ngrams" :depends-on ("utilities"))
               (:file "wordnet" :depends-on ("db-methods"))
               (:file "semantic-similarity" :depends-on ("wordnet"))
               (:file "semantic-similarity-li" :depends-on ("semantic-similarity"))
               (:file "semantic-similarity-raison" :depends-on
                      ("semantic-similarity-li"))
               (:file "pos-tag" :depends-on
                      ("corpora" "lexicon" "db-methods" "sentence-splitter"))
               (:file "grammar" :depends-on ("corpora" "lexicon" "db-methods"))
               (:file "parser" :depends-on ("grammar" "pos-tag"))
               (:file "chunker" :depends-on ("grammar" "pos-tag"))
               (:file "prob-parser" :depends-on ("parser"))
               (:file "pcp" :depends-on ("parser"))
               (:file "initialize" :depends-on
                      ("stop-words" "prob-parser" "pcp" "wordnet" "semantic-similarity-raison"))
               (:file "english" :depends-on ("initialize" "stemmer"))
               (:file "english-lexicon" :depends-on ("english"))
               (:file "spanish" :depends-on
                      ("initialize" "read-cess-esp" "read-ancora-es"
                                    "read-wikicorpus-es" "stemmer"))
               (:file "spanish-lexicon" :depends-on ("spanish"))
               (:file "italian" :depends-on
                      ("initialize" "read-tut" "read-paisa" "stemmer"))
               (:file "portuguese" :depends-on
                      ("initialize" "read-floresta" "stemmer"))
               (:file "german" :depends-on
                      ("initialize" "read-tiger" "stemmer"))
               (:file "french" :depends-on
                      ("initialize" "stemmer"))
               (:file "french-lexicon" :depends-on ("french"))
               ))
