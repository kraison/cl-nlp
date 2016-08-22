(in-package #:cl-user)

(defpackage #:cffi-wordnet
  (:use #:cl #:cffi #:graph-utils)
  (:nicknames #:wordnet)
  (:export #:wordnet-init
	   ;;#:synonyms
           ;;#:hypernyms
           ;;#:holonyms
           #:synset-vertex
           #:make-synset-vertex
           #:morph-word
           #:hypernym-graph
           #:holonym-graph
           #:synset-vertex
           #:word-list
	   #:+noun+
	   #:+verb+
	   #:+adjective+
	   #:+adverb+
           #:+satellite+
	   #:+all-senses+
	   #:+synonyms+
           #:+holonym+
           #:+hypernym+
           #:+meronym+))
