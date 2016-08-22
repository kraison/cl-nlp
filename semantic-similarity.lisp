(in-package :nlp)

(alexandria:define-constant +e+ 2.718281828)

(defvar *similarity-cache*
  (make-hash-table :test 'equalp :synchronized t :weakness nil))
(defvar *distance-cache*
  (make-hash-table :test 'equalp :synchronized t :weakness nil))
(defvar *negative-squared-distance-cache*
  (make-hash-table :test 'equalp :synchronized t :weakness nil))
(defvar *analysis-cache*
  (make-hash-table :test 'equalp :synchronized t :weakness nil))

(defun clear-semantic-caches ()
  (clear-caches-li)
  (clrhash *similarity-cache*)
  (clrhash *distance-cache*)
  (clrhash *negative-squared-distance-cache*)
  (clrhash *analysis-cache*)
  t)

(defun punctuation-p (string)
  (cl-ppcre:scan "^[\\.\\?\\!]+$" string))

(defclass sentence-vertex ()
  ((sentence-number :accessor sentence-number :initform nil
                    :initarg :sentence-number)
   (text :accessor text :initform nil :initarg :text)))

(defmethod print-object ((v sentence-vertex) stream)
  (format stream "#(SENTENCE ~D: ~A)" (sentence-number v) (text v)))

(defclass word-vertex ()
  ((word :accessor word :initform nil :initarg :word)
   (part-of-speech :accessor part-of-speech :initform nil :initarg :part-of-speech)
   (word-position :accessor word-position :initform nil :initarg :word-position)))

(defmethod print-object ((v word-vertex) stream)
  (format stream "#(WORD ~D: ~A/~A)"
          (word-position v) (word v) (part-of-speech v)))

(defclass feature-vertex ()
  ((word :accessor word :initform nil :initarg :word)))

(defmethod print-object ((v feature-vertex) stream)
  (format stream "#(FEATURE ~A)" (word v)))

(defgeneric node-eql (n1 n2)
  (:method (n1 n2)
    (eql n1 n2))
  (:method ((n1 synset-vertex) (n2 synset-vertex))
    (equalp (word-list n1) (word-list n2))))

(sb-ext:define-hash-table-test node-eql sxhash)

(defclass semantic-graph (graph-utils::typed-graph)
  ((word-index :accessor word-index :initform (make-hash-table :test 'equalp))
   (feature-index :accessor feature-index
                  :initform (make-hash-table :test 'equalp))
   (sentences :accessor sentences :initform nil)))

(defun make-semantic-graph ()
  "Create a new semantic-graph object. You are responsible for making sure that
node-comparator is a valid hash table test."
  (let ((g (make-instance 'semantic-graph
                          :comparator 'node-eql
                          :edge-type-comparator 'eql
                          :s-point 0
                          :matrix (make-hash-table :test 'eql)
                          :nodes (make-hash-table :test 'node-eql))))
    g))

(defmethod add-word-vertex ((graph semantic-graph) word part-of-speech position)
  (let ((vertex (add-node graph
                          (make-instance 'word-vertex
                                         :word word
                                         :part-of-speech part-of-speech
                                         :word-position position))))
    (push vertex (gethash word (word-index graph)))
    vertex))

(defmethod add-feature-vertex ((graph semantic-graph) word)
  (let ((vertex (add-node graph (make-instance 'feature-vertex :word word))))
    (setf (gethash word (feature-index graph)) vertex)))

(defmethod add-sentence-vertex ((graph semantic-graph) sentence-number text)
  (let ((vertex (add-node graph (make-instance 'sentence-vertex
                                               :sentence-number sentence-number
                                               :text text))))
    (push vertex (sentences graph))
    vertex))

(defun build-semantic-graph (analysis)
;;  (or (gethash analysis *semantic-graph-cache*)
      (let ((graph (make-semantic-graph))
            (last-sentence-vertex nil))
        (dotimes (i (length analysis))
          (let* ((this-sentence (elt analysis i))
                 (sentence-vertex (add-sentence-vertex
                                   graph i (elt this-sentence 2)))
                 (word-list (elt this-sentence 1))
                 (pos-list (elt this-sentence 0)))
            (when last-sentence-vertex
              (add-edge graph
                        last-sentence-vertex
                        sentence-vertex
                        :edge-type :next-sentence))
            (let ((last-word-vertex nil))
              (dotimes (j (length word-list))
                (let* ((word (elt word-list j))
                       (pos (elt pos-list j))
                       (word-vertex (add-word-vertex graph word pos j)))
                  (when (= j 0)
                    (add-edge graph
                              sentence-vertex
                              word-vertex
                              :edge-type :first-word))
                  (when last-word-vertex
                    (add-edge graph
                              last-word-vertex
                              word-vertex
                              :edge-type :next-word))
                  (when (or (noun-p *language* pos)
                            (adjective-p *language* pos)
                            (verb-p *language* pos))
                    (hypernym-graph word
                                    :graph graph
                                    :root word-vertex
                                    :edge-type :has-parent
                                    :part-of-speech (wordnet-pos pos))
                    (holonym-graph word
                                   :graph graph
                                   :root word-vertex
                                   :edge-type :has-parent
                                   :part-of-speech (wordnet-pos pos)))
                  (setq last-word-vertex word-vertex))))
            (setq last-sentence-vertex sentence-vertex)))
        graph))
;;        (setf (gethash analysis *semantic-graph-cache*) graph))))

(defmethod part-of-speech ((v feature-vertex))
  nil)

(defun hash-table-empty-p (table)
  (assert (typep table 'hash-table))
  (= 0 (hash-table-count table)))

(defun information-content (word)
  (- 1
     (/-safe (log (1+ (word-occurrence *language* word)))
             (log (1+ (total-word-count *language*))))))

(defmethod get-feature-vertex ((graph semantic-graph) feature-word)
  (or (gethash feature-word (feature-index graph))
      (let ((feature-vertex (add-feature-vertex graph feature-word)))
        (dolist (pos (list +noun+ +verb+ +adjective+ +adverb+))
          (hypernym-graph feature-word
                          :part-of-speech pos
                          :graph graph
                          :edge-type :has-parent
                          :root feature-vertex)
          (holonym-graph feature-word
                         :part-of-speech pos
                         :graph graph
                         :edge-type :has-parent
                         :root feature-vertex))
        (setf (gethash feature-word (feature-index graph)) feature-vertex))))

(defun make-numeric-vector (length &optional (default-value 0))
  (make-array (list length)
              :fill-pointer nil
              :adjustable nil
              :element-type 'number
              :initial-element default-value))

(defun do-nlp (text)
  (or (gethash text *analysis-cache*)
      (setf (gethash text *analysis-cache*)
            (tag text))))

(defun semantic-similarity (text1 text2 &key (algorithm :li)
                                          (semantic-weight 0.85d0))
  (let ((key (if (string-lessp text1 text2)
                 (list text1 text2 algorithm semantic-weight)
                 (list text2 text1 algorithm semantic-weight))))
    (case algorithm
      (:raison
       (let ((analysis-1 (do-nlp text1))
             (analysis-2 (do-nlp text2)))
         (semantic-similarity-raison analysis-1 analysis-2)))
      (:li
       (or (gethash key *similarity-cache*)
           (let ((analysis-1 (do-nlp text1))
                 (analysis-2 (do-nlp text2)))
             (setf (gethash key *similarity-cache*)
                   (semantic-similarity-li analysis-1
                                           analysis-2
                                           :semantic-weight semantic-weight))))))))

(defun semantic-distance (text1 text2 &key (algorithm :li)
                                        (semantic-weight 0.85d0))
  (let ((key (if (string-lessp text1 text2)
                 (list text1 text2 algorithm semantic-weight)
                 (list text2 text1 algorithm semantic-weight))))
    (case algorithm
      (:raison
       (let ((analysis-1 (do-nlp text1))
             (analysis-2 (do-nlp text2)))
         (semantic-similarity-raison analysis-1 analysis-2)))
      (:li
       (or (gethash key *distance-cache*)
           (let ((analysis-1 (do-nlp text1))
                 (analysis-2 (do-nlp text2)))
             (setf (gethash key *distance-cache*)
                   (semantic-distance-li analysis-1
                                         analysis-2
                                         :semantic-weight semantic-weight))))))))

(defun negative-squared-semantic-distance (text1 text2
                                           &key (algorithm :li)
                                             (semantic-weight 0.85d0))
  (let ((key (if (string-lessp text1 text2)
                 (list text1 text2 algorithm semantic-weight)
                 (list text2 text1 algorithm semantic-weight))))
    (case algorithm
      ;;      (:raison
      ;;       (let ((analysis-1 (extract-phrases text1))
      ;;             (analysis-2 (extract-phrases text2)))
      ;;         (semantic-similarity-raison analysis-1 analysis-2)))
      (:li
       (or (gethash key *negative-squared-distance-cache*)
           (let ((analysis-1 (do-nlp text1))
                 (analysis-2 (do-nlp text2)))
             (setf (gethash key *negative-squared-distance-cache*)
                   (negative-squared-semantic-distance-li
                    analysis-1
                    analysis-2
                    :semantic-weight semantic-weight))))))))

(defun dump-similarity-cache ()
  (let ((sim-list nil))
    (maphash (lambda (pair distance)
               (push (cons distance pair) sim-list))
             *similarity-cache*)
    (sort sim-list #'> :key 'car)))
