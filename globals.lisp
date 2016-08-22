(in-package #:nlp)

(defvar *pos-db* nil)
(defvar *language* nil)

(defparameter *whitespace* '(#\Space #\Newline #\Return #\Tab))

(defparameter *sentence-start* (intern "<S>" :keyword))
(defparameter *sentence-end* (intern "</S>" :keyword))

(defparameter *punctuation* "([\\.\\!\\?])")
