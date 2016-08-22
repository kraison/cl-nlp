(in-package :nlp)

(alexandria:define-constant +punctuation-tag+ :|.|)
(alexandria:define-constant +foreign-tag+     :X)
(alexandria:define-constant +adjective-tag+   :ADJ)
(alexandria:define-constant +conjunction-tag+ :CONJ)
(alexandria:define-constant +determiner-tag+  :DET)
(alexandria:define-constant +noun-tag+        :NOUN)
(alexandria:define-constant +pronoun-tag+     :PRON)
(alexandria:define-constant +adverb-tag+      :ADV)
(alexandria:define-constant +adposition-tag+  :ADP)
(alexandria:define-constant +verb-tag+        :VERB)
(alexandria:define-constant +number-tag+      :NUM)
(alexandria:define-constant +particle-tag+    :PRT)

(defparameter *part-of-speech-tags*
  (list
   +punctuation-tag+
   +foreign-tag+
   +adjective-tag+
   +conjunction-tag+
   +determiner-tag+
   +noun-tag+
   +pronoun-tag+
   +adverb-tag+
   +adposition-tag+
   +verb-tag+
   +number-tag+
   +particle-tag+)
  "Derived from https://github.com/slavpetrov/universal-pos-tags")

(defun valid-pos-tag-p (tag)
  (member tag *part-of-speech-tags*))

(defgeneric noun-p (language symbol))
(defgeneric verb-p (language symbol))
(defgeneric adjective-p (language symbol))
(defgeneric adverb-p (language symbol))
(defgeneric pronoun-p (language symbol))
(defgeneric determiner-p (language symbol))
