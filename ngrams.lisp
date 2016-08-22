(in-package :nlp)

(defclass ngram-node ()
  ((word :accessor word :initform nil :initarg :word)
   (child-words :accessor child-words :initform (make-hash-table :test 'equal)
                :initarg :child-words)
   (value :accessor value :initform nil :initarg :value)))

(defmethod corpus-ngram-p ((language language) text)
  (scan "^(\\w+_\\w+)+$" text))

(defmethod ngram-components ((language language) text)
  (split "_" text))

(defmethod print-object ((node ngram-node) stream)
  (print-unreadable-object (node stream :type t)
    (format stream "'~A'" (word node))
    (when (value node)
      (format stream ": ~S" (value node)))))

(defun make-ngram-tree ()
  (make-instance 'ngram-node :word :root))

(defun child-word-p (parent word)
  (gethash word (child-words parent)))

(defun maybe-add-ngram-node (parent word)
  (or (child-word-p parent word)
      (let ((node (make-instance 'ngram-node :word word)))
        (setf (gethash word (child-words parent)) node)
        node)))

(defun add-ngram (tree string part-of-speech)
  (let ((ngram (split "_" string))
        (parent tree))
    (dotimes (i (length ngram))
      (let ((node (maybe-add-ngram-node parent (elt ngram i))))
        (if (= i (- (length ngram) 1))
            (pushnew part-of-speech (value node))
            (setq parent node))))
    tree))

(defun ngram-p (tree words)
  (labels
      ((walk-tree (parent word-list)
         (if (null word-list)
             (value parent)
             (let ((child (child-word-p parent (first word-list))))
               (when child
                 (walk-tree child (rest word-list)))))))
    (walk-tree tree words)))

(defmethod extract-skip-bigrams ((language nlp:language) (tokens sequence)
                                 &key (skip 2) stem-p)
  (let ((skip-grams (make-hash-table :test 'equalp))
        (tokens-length (length tokens)))
    (loop for i from 0 below tokens-length do
         (loop for j from (1+ i) to (+ 1 skip i) do
              (unless (>= j tokens-length)
                (let ((token1 (if stem-p
                                  (nlp:stem language (elt tokens i))
                                  (elt tokens i)))
                      (token2 (if stem-p
                                  (nlp:stem language (elt tokens j))
                                  (elt tokens j))))
                  (unless (equalp token1 token2)
                    (incf (gethash
                           (if (string-lessp token1 token2)
                               (list token1 token2)
                               (list token2 token1))
                           skip-grams 0)))))))
    skip-grams))

(defmethod extract-skip-trigrams ((language nlp:language) (tokens sequence)
                                  &key (skip 2) stem-p)
  ;; FIXME: implement
  )

(defmethod extract-ngrams ((tokens sequence) &key include-quadrigrams-p)
  (let ((sentence-length (length tokens))
        (total-unigrams 0)
        (total-bigrams 0)
        (total-trigrams 0)
        (total-quadrigrams 0)
        (ngram-table (make-hash-table :test 'equalp)))
    (loop for i from 0 below sentence-length do
       ;; get the unigrams
         (incf total-unigrams)
         (incf (gethash (list (elt tokens i)) ngram-table 0))
       ;; get the bigrams
         (when (< i (- sentence-length 1))
           (incf total-bigrams)
           (incf (gethash (list (elt tokens i)
                                (elt tokens (1+ i)))
                          ngram-table 0))
           ;; get the trigrams
           (when (< i (- sentence-length 2))
             (incf total-trigrams)
             (incf (gethash (list (elt tokens i)
                                  (elt tokens (1+ i))
                                  (elt tokens (+ 2 i)))
                            ngram-table 0))
             ;; get the quadrigrams if requested
             (when (and include-quadrigrams-p
                        (< i (- sentence-length 3)))
               (incf total-quadrigrams)
               (incf (gethash (list (elt tokens i)
                                    (elt tokens (1+ i))
                                    (elt tokens (+ 2 i))
                                    (elt tokens (+ 3 i)))
                              ngram-table 0))))))
    (values ngram-table
            total-unigrams
            total-bigrams
            total-trigrams
            total-quadrigrams)))
