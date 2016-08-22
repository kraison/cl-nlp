(in-package :nlp)

(defun singularize (word)
  "Singularize a word using Wordnet.  Assumes you have already called
(cffi-wordnet:wordnet-init)"
  (let ((singular (cffi-wordnet:morph-word word cffi-wordnet:+noun+)))
    (or singular word)))

(defun verb-base-form (verb)
  "Find base form of the verb. Assumes you have already called
(cffi-wordnet:wordnet-init)"
  (let ((base (cffi-wordnet:morph-word verb cffi-wordnet:+verb+)))
    (or base verb)))
