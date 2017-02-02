(in-package :nlp)

(defmethod print-object ((language language) stream)
  (print-unreadable-object (language stream)
    (format stream
            "~A (~A): ~A bigrams, ~A trigrams ~A observation likelihoods"
            (type-of language)
            (name language)
            (hash-table-count (bigrams language))
            (hash-table-count (trigrams language))
            (hash-table-count (observations language)))))

(defgeneric language-p (language)
  (:method ((language language)) t)
  (:method (thing) nil))

(defmethod part-of-speech-tags ((language language))
  (alexandria:hash-table-keys (unigrams language)))

(defmethod add-language ((language language))
  (setf (gethash (name language) *languages*) language)
  (dolist (alias (aliases language))
    (setf (gethash alias *languages*) language))
  language)

(defun lookup-language (name)
  (gethash name *languages*))

(defmethod set-language-finalizers ((language language))
  "Make sure the Kyoto Cabinet references are properly closed if this instance
is garbage collected."
  (let* ((lexicon-dbm (lexicon-dbm language))
         (plexicon-dbm (plexicon-dbm language))
         (observations-dbm (observations-dbm language))
         (word-occurrence-dbm (word-occurrence-dbm language)))
    #+sbcl
    (sb-ext:finalize language
                     (lambda ()
                       (when (typep word-occurrence-dbm 'kc-dbm)
                         (dbm-close word-occurrence-dbm))
                       (when (typep observations-dbm 'kc-dbm)
                         (dbm-close observations-dbm))
                       (when (typep lexicon-dbm 'kc-dbm)
                         (dbm-close lexicon-dbm))
                       (when (typep plexicon-dbm 'kc-dbm)
                         (dbm-close plexicon-dbm))))
    language))

(defmethod make-new-language ((class symbol))
  "Constructor that takes care of creating language objects and lexicon dbms"
  (let ((language (make-instance class)))
    (make-word-occurrence-dbm language)
    (make-observations-dbm language)
    (make-lexicon-dbm language)
    (make-plexicon-dbm language)
    (set-language-finalizers language)
    language))

(defmethod close-language ((language language))
  (close-word-occurrence-dbm language)
  (close-observations-dbm language)
  (close-lexicon-dbm language)
  (close-plexicon-dbm language)
  (remhash (name language) *languages*)
  (dolist (alias (aliases language))
    (remhash alias *languages*)))

(defmethod noun-p ((language language) (word string))
  (some (lambda (pos-tag)
          (noun-p language pos-tag))
        (nlp:lookup-pos language word)))

(defmethod adjective-p ((language language) (word string))
  (some (lambda (pos-tag)
          (adjective-p language pos-tag))
        (nlp:lookup-pos language word)))

(defmethod verb-p ((language language) (word string))
  (some (lambda (pos-tag)
          (verb-p language pos-tag))
        (nlp:lookup-pos language word)))

(defmethod adverb-p ((language language) (word string))
  (some (lambda (pos-tag)
          (adverb-p language pos-tag))
        (nlp:lookup-pos language word)))

(defmethod determiner-p ((language language) (word string))
  (some (lambda (pos-tag)
          (determiner-p language pos-tag))
        (nlp:lookup-pos language word)))

(defmethod pronoun-p ((language language) (word string))
  (some (lambda (pos-tag)
          (pronoun-p language pos-tag))
        (nlp:lookup-pos language word)))
