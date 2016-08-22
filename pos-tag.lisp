(in-package #:nlp)

(defmethod word-occurrence ((language language) word)
  "How many times was word seen in the corpus?"
  ;;(gethash word (word-occurrences language) 0))
  (lookup-word-occurrence language word))

(defmethod total-word-count ((language language))
  (total-count language))

(defmethod add-word-occurrence ((language language) word)
  "See the word again"
  ;;(init-or-increment (word-occurrences language) word))
  (incf-word-occurrence language word))

(defmethod add-unigram ((language language) unigram)
  "Add a unigram to the db"
  (incf (total-count language))
  (init-or-increment (unigrams language) unigram))

(defmethod get-unigram ((language language) unigram)
  "Return a unigram's count"
  (or (gethash unigram (unigrams language)) 0))

(defmethod add-bigram ((language language) bigram)
  "Add a bigram to the db"
  (init-or-increment (bigrams language) bigram))

(defmethod get-bigram ((language language) bigram)
  "Return a bigram's count"
  (or (gethash bigram (bigrams language)) 0))

(defmethod add-trigram ((language language) trigram)
  "Add a trigram to the db"
  (init-or-increment (trigrams language) trigram))

(defmethod get-trigram ((language language) trigram)
  "Return the trigram's count"
  (or (gethash trigram (trigrams language)) 0))

(defmethod calculate-gammas ((language language))
  "Calculate gammas for deleted interpolation"
  (let ((g1 0) (g2 0) (g3 0))
    (maphash
     (lambda (trigram count)
       (let* ((p3 (/-safe (1- count)
                          (1- (get-bigram language (subseq trigram 0 2)))))
              (p2 (/-safe (1- (get-bigram language (subseq trigram 1 3)))
                          (1- (get-unigram language (second trigram)))))
              (p1 (/-safe (1- (get-unigram language (third trigram)))
                          (1- (total-count language)))))
         (cond ((> p3 p2 p1)
                (setq g3 (+ g3 count)))
               ((> p2 p3 p1)
                (setq g2 (+ g2 count)))
               ((> p1 p2 p3)
                (setq g1 (+ g1 count))))))
     (trigrams language))
    (let ((total (+ g1 g2 g3)))
      (values (setf (gethash 1 (gammas language)) (/-safe g1 total))
              (setf (gethash 2 (gammas language)) (/-safe g2 total))
              (setf (gethash 3 (gammas language)) (/-safe g3 total))))))

(defmethod compute-trigram-probability ((language language) trigram)
  "Compute trigram probability"
  (+ (* (gethash 3 (gammas language))
        (/-safe (gethash trigram (trigrams language))
                (get-bigram language (subseq trigram 0 2))))
     (* (gethash 2 (gammas language))
        (/-safe (get-bigram language (subseq trigram 1 3))
                (get-unigram language (second trigram))))
     (* (gethash 1 (gammas language))
        (/-safe (get-unigram language (third trigram))
                (total-count language)))))

(defmethod compute-ngram-probabilities ((language language))
  "Compute and store gammas and probabilities for all Ngrams in the db"
  (calculate-gammas language)
  (maphash
   (lambda (trigram count)
     (declare (ignore count))
     (setf (gethash trigram (probabilities language))
           (compute-trigram-probability language trigram)))
   (trigrams language)))

(defmethod trigram-probability ((language language) trigram)
  "Lookup trigram probability"
  (let ((p (gethash trigram (probabilities language))))
    (if (and (numberp p) (> p 0))
        p
        (compute-trigram-probability language trigram))))

(defmethod add-observation ((language language) word pos)
  "Add an observation for WORD"
  (let ((observation (cdr (assoc pos (lookup-observations-dbm language word)))))
    (if observation
        (add-observation-dbm language word pos (1+ observation))
        (add-observation-dbm language word pos 1))))
#|
  (unless (hash-table-p (gethash word (observations language)))
    (setf (gethash word (observations language))
          (make-hash-table :test 'equal)))
  (if (numberp (gethash pos (gethash word (observations language))))
      (incf (gethash pos (gethash word (observations language))))
      (setf (gethash pos (gethash word (observations language))) 1)))
|#

(defmethod get-observation ((language language) word pos)
  "return observations of WORD as POS"
  (or (cdr (assoc pos (lookup-observations-dbm language word)))
      (unknown-probability language)))
#|
  (let ((table (gethash word (observations language))))
    (if (hash-table-p table)
        (gethash pos table 0)
        (unknown-probability language))))
|#

(defmethod get-observations ((language language) word)
  "Get all observations for WORD"
  (lookup-observations-dbm language word))
#|
  (let ((table (gethash word (observations language))))
    (if (hash-table-p table)
        (let ((o nil))
          (maphash (lambda (pos p)
                     (push (cons pos p) o))
                   table)
          (sort o '> :key 'cdr))
        nil)))
|#

(defmethod compute-observation-likelihoods ((language language))
  "Compute all word / POS observation likelihoods"
  (setf (unknown-probability language)
        (/-safe 1 (hash-table-count (unigrams language))))
  (let ((iterator (iter-open (observations-dbm language))))
    (unwind-protect
         (progn
           (iter-first iterator)
           (loop
              (multiple-value-bind (word-vector observations-vector)
                  (iter-item iterator :value-type :octets :key-type :octets)
                (when (zerop (length word-vector))
                  (return))
                (let ((pos-seq (deserialize-observations-seq observations-vector))
                      (word (sb-ext:octets-to-string word-vector)))
                  (replace-observations-dbm
                   language
                   word
                   (mapcar (lambda (pair)
                             (setf (cdr pair)
                                   (/-safe (cdr pair)
                                           (get-unigram language (car pair))))
                             pair)
                           pos-seq))))
              (iter-next iterator)))
      (iter-close iterator))
    nil))
#|
  (maphash (lambda (word table)
             (declare (ignore word))
             (maphash (lambda (pos count)
                        (setf (gethash pos table)
                              (/-safe count (get-unigram language pos))))
                      table))
           (observations language)))
|#

(defmethod increment-sentence-markers ((language language))
  "Add sentence markers to db for a new sentence"
  (add-unigram language *sentence-start*)
  (add-unigram language *sentence-end*)
  (add-word-occurrence language "<s>")
  (add-word-occurrence language "</s>")
  (add-bigram language (list *sentence-start* *sentence-start*))
  (add-bigram language (list *sentence-end* *sentence-end*))
  (add-observation language "<s>" *sentence-start*)
  (add-observation language "</s>" *sentence-end*))

(defmethod train-tagger ((language language) file &key (external-format :utf-8))
  "Train the POS tagger against the corpus FILE. Augment unigrams with lexicon entries."
  (let ((sentence nil) (pos-seq nil))
    (map-lexicon
     (lambda (word pos-list)
       (dolist (pos pos-list)
         (add-unigram language pos)
         (add-word-occurrence language word)
         (add-observation language word pos)))
     language)
    (map-tagged-corpus
     (lambda (word pos)
       (add-unigram language pos)
       (add-word-occurrence language word)
       (add-observation language word pos)
       (push pos pos-seq)
       (push word sentence)
       (when (end-of-sentence-p word pos)
         (unless (null pos-seq)
           (let ((pos-seq (nreverse pos-seq)))
             (increment-sentence-markers language)
             (add-bigram language
                         (list *sentence-start* (first pos-seq)))
             (add-bigram language
                         (list (last1 pos-seq) *sentence-end*))
             (add-trigram language (list *sentence-start*
                                         *sentence-start*
                                         (first pos-seq)))
             (add-trigram language (list (last1 pos-seq)
                                         *sentence-end*
                                         *sentence-end*))
             (add-trigram language
                          (list *sentence-start*
                                (first pos-seq)
                                (second pos-seq)))
             (add-trigram language (list (second-to-last pos-seq)
                                         (last1 pos-seq)
                                         *sentence-end*))
             (loop for i from 1 to (1- (length pos-seq)) do
                  (add-bigram language (list (elt pos-seq (1- i))
                                             (elt pos-seq i)))
                  (when (> i 1)
                    (add-trigram language (list (elt pos-seq (- i 2))
                                                (elt pos-seq (1- i))
                                                (elt pos-seq i)))))))
         (setq sentence nil pos-seq nil)))
     file
     :external-format external-format)
    (compute-observation-likelihoods language)
    (compute-ngram-probabilities language)
    language))

(defmethod possible-states ((language language) words)
  "Return all possible POS tags for WORDS"
  (let ((states nil))
    (dolist (word words)
      (let ((w-states (lookup-pos language word)))
        (dolist (w-state w-states)
          (pushnew w-state states :test 'equal))))
    states))

(defun calculate-path (v words states)
  "Reconstruct the path from the viterbi matrix"
  (let ((path nil))
    (dotimes (j (length words))
      (let ((max 0) (state nil))
        (dotimes (i (length states))
          (when (> (aref v i j) max)
            (setq max (aref v i j)
                  state (elt states i))))
        (push state path)))
    (nreverse path)))

(defmethod possible-tags ((language language) text &key p?)
  "Get all possible tags for all words in TEXT. If P?, return probabilities too"
  (let ((words (if (stringp text) (tokenize language text) text)))
    (if p?
        (values (mapcar (lambda (word)
                          (pos-probabilities language word))
                        words) words)
        (values (mapcar (lambda (word)
                          (lookup-pos language word))
                        words) words))))

(defun tag-sentence (words &key (language *language*))
  "POS tag an individual sentence"
  (let* ((words (if (stringp words) (tokenize language words) words))
         (states (possible-states language words))
         (viterbi (make-array (list (length states) (length words))
                              :initial-element 0)))
    ;;(log:debug "Doing word '~A'" (elt words 0))
    (dotimes (i (length states))
      (let ((p (* (trigram-probability language (list *sentence-start*
                                                      *sentence-start*
                                                      (elt states i)))
                  (get-observation language (first words) (elt states i)))))
        ;;(when (> p 0)
        ;;  (log:debug "   ~A: trigram: (<s> <s> ~A): ~F"
        ;;             (elt words 0)
        ;;             (elt states i)
        ;;             (trigram-probability language
        ;;                                  (list *sentence-start*
        ;;                                        *sentence-start*
        ;;                                        (elt states i)))))
        (setf (aref viterbi i 0) p)))
    (loop for j from 1 below (length words) do
         ;;(log:debug "Doing word '~A'" (elt words j))
         (dotimes (i (length states))
           ;; Efficiency hack;  improves accuracy when lexicon is integrated with
           ;; unigrams, kills accuracy when lexicon is not in use
           (when (or (null (lookup-pos language (elt words j)))
                     (is-a (elt words j) (elt states i) language))
             (setf (aref viterbi i j)
                   (* (let ((pp nil))
                        (dotimes (i1 (length states))
                          (dotimes (i2 (length states))
                            (unless (= 0 (aref viterbi i2 (1- j)))
                              (let ((p (* (aref viterbi i2 (1- j))
                                          (trigram-probability
                                           language
                                           (list (elt states i1)
                                                 (elt states i2)
                                                 (elt states i))))))
                                ;;(when (> p 0)
                                ;;  (log:debug "   ~A: trigram (~A ~A ~A): ~F"
                                ;;             (elt words j)
                                ;;             (elt states i1)
                                ;;             (elt states i2)
                                ;;             (elt states i)
                                ;;             p))
                                (push p pp)))))
                        (if pp (apply 'max pp) 0))
                      (get-observation language (elt words j) (elt states i)))))))
    (values (calculate-path viterbi words states) words)))

(defun tag (text &optional (language *language*))
  "Split TEXT into sentences and tag each one"
  (mapcar (lambda (sentence)
            (nconc
             (multiple-value-list
              (tag-sentence sentence :language language))
             (list sentence)))
          (split-sentences language text)))

(defun tag-as-text (text &optional (language *language*))
  "Tag text and return in WORD/TAG format"
  (let ((tags (tag text language)))
    (format nil "~{~A~^ ~}"
            (mapcar (lambda (i)
                      (format nil "~{~A~^ ~}"
                              (mapcar (lambda (tag word)
                                        (format nil "~A/~A" word tag))
                                      (first i) (second i))))
                    tags))))

(defun dump-unigrams (&optional (language *language*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
             (unigrams language))
    r))

(defun dump-bigrams (&optional (language *language*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
             (bigrams language))
    (sort r 'string> :key 'caar)))

(defun dump-trigrams (&optional (language *language*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
             (trigrams language))
    (sort r 'string> :key 'caddar)))

(defun dump-probabilities (&optional (language *language*))
  (let ((r nil))
    (maphash (lambda (ngram p)
               (push (cons ngram p) r))
             (probabilities language))
    (sort r '> :key 'cdr)))

(defun dump-pos-observations (&optional (language *language*))
  (let ((r nil)
        (iterator (iter-open (observations-dbm language))))
    (unwind-protect
         (progn
           (iter-first iterator)
           (loop
              (multiple-value-bind (word-vector observations-vector)
                  (iter-item iterator :value-type :octets :key-type :octets)
                (when (zerop (length word-vector))
                  (return))
                (let ((pos-seq (deserialize-observations-seq observations-vector))
                      (word (sb-ext:octets-to-string word-vector)))
                  (map nil
                       (lambda (pair)
                         (push `((,word ,(car pair)) ,(cdr pair)) r))
                       pos-seq)))
              (iter-next iterator)))
      (iter-close iterator))
    (sort r '> :key 'second)))
#|
    (maphash (lambda (word table)
               (maphash (lambda (pos p)
                          (push `((,word ,pos) ,p) r))
                        table))
             (observations language))
    (sort r '> :key 'second)))
|#
