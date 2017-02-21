(in-package :nlp)

(alexandria:define-constant +noun-phrase+ :NP)
(alexandria:define-constant +verb-phrase+ :VP)
(alexandria:define-constant +noun-phrase-begin+ :NP-BEGIN)
(alexandria:define-constant +noun-phrase-in+ :NP-IN)

(defmethod reset-chunker-tables ((language language))
  (setf (np-total-count language) 0
        (np-gammas language) (make-hash-table)
        (np-probabilities language) (make-hash-table :test 'equal)
        (np-unigrams language) (make-hash-table :test 'equal)
        (np-bigrams language) (make-hash-table :test 'equal)
        (np-trigrams language) (make-hash-table :test 'equal)
        (np-tag-occurrences language) (make-hash-table :test 'equal)
        (np-pos-occurrences language) (make-hash-table :test 'equal)
        (np-observations language) (make-hash-table :test 'equal)
        (np-unknown-probability language) 0)
  language)

(defun flatten-phrase-tree (tree)
  (let ((leaf-phrases nil))
    (labels ((dfs-helper (this-node phrase-type)
               (cond ((and (consp this-node)
                           (= 2 (length this-node))
                           (atom (first this-node))
                           (atom (second this-node)))
                      (push (list phrase-type (first this-node)) leaf-phrases))
                     ((and (consp this-node)
                           (atom (first this-node))
                           (every 'consp (rest this-node))
                           (every (lambda (c) (= 2 (length c))) (rest this-node)))
                      (dolist (phrase (rest this-node))
                        (dfs-helper phrase (first this-node))))
                     ((consp this-node)
                      (dolist (child this-node)
                        (dfs-helper child (first this-node)))))))
      (dfs-helper tree nil))
    (nreverse leaf-phrases)))

(defmethod np-tag-occurrence ((language language) tag)
  "How many time was tag seen in the corpus?"
  (gethash tag (np-tag-occurrences language)))

(defmethod add-np-tag-occurrence ((language language) tag)
  "See the tag again"
  (init-or-increment (np-tag-occurrences language) tag))

(defmethod add-np-unigram ((language language) unigram)
  "Add a unigram to the db"
  (incf (np-total-count language))
  (init-or-increment (np-unigrams language) unigram))

(defmethod get-np-unigram ((language language) unigram)
  "Return a unigram's count"
  (gethash unigram (np-unigrams language) 0))

(defmethod add-np-bigram ((language language) bigram)
  "Add a bigram to the db"
  (init-or-increment (np-bigrams language) bigram))

(defmethod get-np-bigram ((language language) bigram)
  "Return a bigram's count"
  (gethash bigram (np-bigrams language) 0))

(defmethod add-np-trigram ((language language) trigram)
 "Add a trigram to the db"
  (init-or-increment (np-trigrams language) trigram))

(defmethod get-np-trigram ((language language) trigram)
  "Return the trigram's count"
  (gethash trigram (np-trigrams language) 0))

(defmethod add-np-observation ((language language) pos-tag np-tag)
  "Add an observation for pos-tag"
  (unless (hash-table-p (gethash pos-tag (np-observations language)))
    (setf (gethash pos-tag (np-observations language))
          (make-hash-table :test 'equal)))
  (if (numberp (gethash np-tag (gethash pos-tag (np-observations language))))
      (incf (gethash np-tag (gethash pos-tag (np-observations language))))
      (setf (gethash np-tag (gethash pos-tag (np-observations language))) 1)))

(defmethod get-np-observation ((language language) pos-tag np-tag)
  "return observations of pos-tag as np-tag"
  (let ((table (gethash pos-tag (np-observations language))))
    (if (hash-table-p table)
        (gethash np-tag table 0)
        (np-unknown-probability language))))

(defmethod get-np-observations ((language language) pos-tag)
  "Get all observations for pos-tag"
  (let ((table (gethash pos-tag (np-observations language))))
    (if (hash-table-p table)
        (let ((o nil))
          (maphash (lambda (pos p)
                     (push (cons pos p) o))
                   table)
          (sort o '> :key 'cdr))
        nil)))

(defmethod compute-np-observation-likelihoods ((language language))
  "Compute all pos-tag / np-tag observation likelihoods"
  (setf (np-unknown-probability language)
        (/-safe 1 (hash-table-count (np-unigrams language))))
  (maphash (lambda (pos-tag table)
             (declare (ignore pos-tag))
             (maphash (lambda (np-tag count)
                        (setf (gethash np-tag table)
                              (/ count (get-np-unigram language np-tag))))
                      table))
           (np-observations language)))

(defmethod calculate-np-gammas ((language language))
  "Calculate gammas for deleted interpolation"
  (let ((g1 0) (g2 0) (g3 0))
    (maphash
     (lambda (trigram count)
       (let* ((p3 (/-safe (1- count)
                          (1- (get-np-bigram language (subseq trigram 0 2)))))
              (p2 (/-safe (1- (get-np-bigram language (subseq trigram 1 3)))
                          (1- (get-np-unigram language (second trigram)))))
              (p1 (/-safe (1- (get-np-unigram language (third trigram)))
                          (1- (np-total-count language)))))
         (cond ((> p3 p2 p1)
                (setq g3 (+ g3 count)))
               ((> p2 p3 p1)
                (setq g2 (+ g2 count)))
               ((> p1 p2 p3)
                (setq g1 (+ g1 count))))))
     (np-trigrams language))
    (let ((total (+ g1 g2 g3)))
      (values (setf (gethash 1 (np-gammas language)) (/-safe g1 total))
              (setf (gethash 2 (np-gammas language)) (/-safe g2 total))
              (setf (gethash 3 (np-gammas language)) (/-safe g3 total))))))

(defmethod compute-np-trigram-probability ((language language) trigram)
  "Compute trigram probability"
  (+ (* (gethash 3 (np-gammas language))
        (/-safe (gethash trigram (np-trigrams language))
                (get-np-bigram language (subseq trigram 0 2))))
     (* (gethash 2 (np-gammas language))
        (/-safe (get-np-bigram language (subseq trigram 1 3))
                (get-np-unigram language (second trigram))))
     (* (gethash 1 (np-gammas language))
        (/-safe (get-np-unigram language (third trigram))
                (np-total-count language)))))

(defmethod np-trigram-probability ((language language) trigram)
  "Lookup np-trigram probability"
  (let ((p (gethash trigram (np-probabilities language))))
    (if (and (numberp p) (> p 0))
        p
        (compute-np-trigram-probability language trigram))))

(defmethod compute-np-ngram-probabilities ((language language))
  "Compute and store gammas and probabilities for all Ngrams in the db"
  (calculate-np-gammas language)
  (maphash
   (lambda (trigram count)
     (declare (ignore count))
     (setf (gethash trigram (np-probabilities language))
           (compute-np-trigram-probability language trigram)))
   (np-trigrams language)))

(defun translate-phrase-marker (marker)
  (let ((string (symbol-name marker)))
    (cond ((or (eql (search "NP" string) 0)
               (eql (search "NX" string) 0)
               (eql (search "PN" string) 0)
               (eql (search "UCP" string) 0)
               (eql (search "GRUP.NOM" string) 0)
               ;;(eql (search "WHNP" string) 0)
               (eql (search "NAC" string) 0))
           +noun-phrase+)
          ((or (eql (search "VP" string) 0)
               (eql (search "VN" string) 0)
               (eql (search "GRUP.VERB" string) 0))
           +verb-phrase+)
#|
          ((search "ADJ" string)
           :ADJP)
          ((search "ADV" string)
           :ADVP)
          ((or (eql (search "PP" string) 0)
               (eql (search "WHPP" string) 0))
           :PP)
          ((eql (search "CONJ" string) 0)
           :CONJP)
          ((eql (search "INT" string) 0)
           :INTP)
          ((eql (search "FRAG" string) 0)
           :FRAG)
          ((equal "PRT" string)
           :PRT)
          ((eql (search "WH" string) 0)
           :WH)
          ((or (equal "S" string)
               (eql (search "SBAR" string) 0)
               (eql (search "S" string) 0))
           :S)
|#
          (t
           :OTHER))))

(defun increment-np-sentence-markers (language)
  "Add sentence markers to db for a new sentence"
  (add-np-unigram language *sentence-start*)
  (add-np-unigram language *sentence-end*)
  (add-np-tag-occurrence language "<s>")
  (add-np-tag-occurrence language "</s>")
  (add-np-bigram language (list *sentence-start* *sentence-start*))
  (add-np-bigram language (list *sentence-end* *sentence-end*))
  (add-np-observation language "<s>" *sentence-start*)
  (add-np-observation language "</s>" *sentence-end*))

(defun make-begin-end-markers (seq)
  (let ((in-phrase nil) (new-seq nil))
    (dolist (marker seq)
      (cond ((eql in-phrase marker)
             (push (intern (format nil "~A-IN" marker) :keyword) new-seq))
            (t
             (setq in-phrase marker)
             (push (intern (format nil "~A-BEGIN" marker) :keyword) new-seq))))
    (nreverse new-seq)))

(defmethod learn-phrase-pattern ((language language) pattern)
  (let ((pos-seq (mapcar 'second pattern))
        (marker-seq (make-begin-end-markers
                     (mapcar (lambda (pair)
                               (translate-phrase-marker (first pair)))
                             pattern))))
    (dotimes (i (length pos-seq))
      (add-np-unigram language (elt marker-seq i))
      (add-np-tag-occurrence language (elt pos-seq i))
      (add-np-observation language (elt pos-seq i) (elt marker-seq i)))
    (increment-np-sentence-markers language)
    (add-np-bigram language
                   (list *sentence-start* (first marker-seq)))
    (add-np-bigram language
                   (list (last1 marker-seq) *sentence-end*))
    (add-np-trigram language (list *sentence-start*
                                   *sentence-start*
                                   (first marker-seq)))
    (add-np-trigram language (list (last1 marker-seq)
                                   *sentence-end*
                                   *sentence-end*))
    (add-np-trigram language
                    (list *sentence-start*
                          (first marker-seq)
                          (second marker-seq)))
    (add-np-trigram language (list (second-to-last marker-seq)
                                   (last1 marker-seq)
                                   *sentence-end*))
    (loop for i from 1 to (1- (length marker-seq)) do
         (add-np-bigram language (list (elt marker-seq (1- i))
                                       (elt marker-seq i)))
         (when (> i 1)
           (add-np-trigram language (list (elt marker-seq (- i 2))
                                          (elt marker-seq (1- i))
                                          (elt marker-seq i)))))))

(defun flatten-parsed-corpus (in-file out-file)
  (with-open-file (out out-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (map-sexp-corpus
     (lambda (tree)
       (format out "~S" (flatten-phrase-tree tree)))
     in-file)))

(defmethod train-phrase-extractor ((language language) file)
  "Train the noun phrase extractor on a given labeled corpus."
  (reset-chunker-tables language)
  (map-sexp-corpus
   (lambda (tree)
     (let ((flattened-tree (flatten-phrase-tree tree)))
       (learn-phrase-pattern language flattened-tree)))
   file)
  (compute-np-observation-likelihoods language)
  (compute-np-ngram-probabilities language)
  language)

(defun possible-np-states (language pos-tags)
  "Return all possible phrase tags for WORDS"
  (let ((states nil))
    (dolist (pos-tag pos-tags)
      (let ((w-states (mapcar 'car (get-np-observations language pos-tag))))
        (dolist (w-state w-states)
          (pushnew w-state states :test 'equal))))
    states))

;;; FIXME: this code needs refactoring to remove the pos symbols;  may be helped
;;; by using the 12 generic symbols defined in pos-symbols, and mapping the
;;; language in use to the generic symbols.  If there are variations on a per-
;;; language basis, specialized methods will need to be written
(defmethod reconstruct-phrases ((language language) markers words pos-tags)
  "Convert NP markers to lists of noun phrase words."
  (let ((phrases nil) (this-phrase nil) (in-phrase nil) (tags nil) (these-tags nil))
    (dotimes (i (length markers))
      (cond ((and (member (elt markers i)
                          (list +noun-phrase-begin+ +noun-phrase-in+))
                  (not (member (elt pos-tags i) '(:PRON :X :DET :|.| :FS :RD :RI :PE))))
             (if (and in-phrase
                      (not (member (elt pos-tags i) '(:CC :CONJ :CS))))
                 (progn
                   (push (elt words i) this-phrase)
                   (push (elt pos-tags i) these-tags))
                 (progn
                   (when this-phrase
                     (push (nreverse this-phrase) phrases)
                     (push (nreverse these-tags) tags)
                     (setq this-phrase nil)
                     (setq these-tags nil))
                   (when (not (member (elt pos-tags i) '(:CC :CONJ :CS)))
                     (push (elt words i) this-phrase)
                     (push (elt pos-tags i) these-tags))
                   (setq in-phrase t))))
            (t
             (when (and in-phrase this-phrase)
               (unless (and (= 1 (length this-phrase))
                            (member (elt these-tags 0) '(:POS :JJ :PRT :ADJ :PP :A :AP)))
                 (push (nreverse this-phrase) phrases)
                 (push (nreverse these-tags) tags))
               (setq in-phrase nil
                     these-tags nil
                     this-phrase nil)))))
    (when (and in-phrase this-phrase)
      (unless (and (= 1 (length this-phrase))
                   (member (elt these-tags 0) '(:POS :JJ :PRT :ADJ :PP :A :AP)))
        (push (nreverse this-phrase) phrases)
        (push (nreverse these-tags) tags)))
    (values (nreverse phrases)
            (nreverse tags))))

(defmethod extract-phrases ((language language) sentence)
  "Extract noun phrases from an individual sentence using Viterbi / HMM strategy."
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (pos-tags words)
      (tag-sentence sentence :language language)
    (let* ((states (possible-np-states language pos-tags))
           (viterbi (make-array (list (length states) (length pos-tags))
                                :element-type 'float
                                :initial-element 0.0)))
      ;;(log:debug "TAGS: ~S" pos-tags)
      ;;(log:debug "STATES: ~S" states)
      ;;(log:debug "EXTRACT-PHRASES: Doing word '~A/~A'"
      ;;          (elt words 0) (elt pos-tags 0))
      (dotimes (i (length states))
        (let ((p (* (np-trigram-probability language (list *sentence-start*
                                                         *sentence-start*
                                                         (elt states i)))
                    (get-np-observation language
                                        (first pos-tags)
                                        (elt states i)))))
          ;;(when (> p 0)
          ;;  (log:debug "   ~A: trigram: (<s> <s> ~A): ~F"
          ;;             (elt pos-tags 0)
          ;;             (elt states i)
          ;;             (np-trigram-probability language
          ;;                                     (list *sentence-start*
          ;;                                           *sentence-start*
          ;;                                           (elt states i)))))
          (setf (aref viterbi i 0) p)))
      (loop for j from 1 to (1- (length pos-tags)) do
           ;;(log:debug "Doing tag '~A'" (elt pos-tags j))
           (dotimes (i (length states))
             (setf (aref viterbi i j)
                   (* (let ((pp nil))
                        (dotimes (i1 (length states))
                          (dotimes (i2 (length states))
                            (unless (= 0 (aref viterbi i2 (1- j)))
                              (let ((p (* (aref viterbi i2 (1- j))
                                          (np-trigram-probability
                                           language
                                           (list (elt states i1)
                                                 (elt states i2)
                                                 (elt states i))))))
                                ;;(when (> p 0)
                                ;;  (log:debug "   ~A: trigram (~A ~A ~A): ~F"
                                ;;          (elt pos-tags j)
                                ;;          (elt states i1)
                                ;;          (elt states i2)
                                ;;          (elt states i)
                                ;;          p))
                                (push p pp)))))
                        (if pp (apply 'max pp) 0))
                      (get-np-observation language
                                          (elt pos-tags j)
                                          (elt states i))))))
      (let ((markers (calculate-path viterbi pos-tags states)))
        (multiple-value-bind (phrases tags)
            (reconstruct-phrases language markers words pos-tags)
          (values phrases
                  tags
                  markers
                  pos-tags
                  words))))))

(defmethod all-phrases ((language language) text)
  "Split TEXT into sentences and extract phrases from each one"
  (mapcar (lambda (sentence)
            (nconc
             (multiple-value-list
              (extract-phrases language sentence))
             (list sentence)))
          (split-sentences language text)))
