(in-package :nlp)

(defclass synset ()
  ((synset-id :accessor synset-id :initform nil :initarg :synset-id)
   (synset-type :accessor synset-type :initform nil :initarg :synset-type)
   (sense-number :accessor sense-number :initform nil :initarg :sense-number)
   (tag-count :accessor tag-count :initform nil :initarg :tag-count)
   (synset-gloss :accessor synset-gloss :initform nil :initarg :synset-gloss)
   (hypernym-ptrs :accessor hypernym-ptrs :initform nil :initarg :hypernym-ptrs)
   (hyponym-ptrs :accessor hyponym-ptrs :initform nil :initarg :hyponym-ptrs)
   (instance-ptrs :accessor instance-ptrs :initform nil :initarg :instance-ptrs)
   (entailment-ptrs :accessor entailment-ptrs :initform nil :initarg :entailment-ptrs)
   (similar-ptrs :accessor similar-ptrs :initform nil :initarg :similar-ptrs)
   (member-meronym-ptrs :accessor member-meronym-ptrs :initform nil :initarg :member-meronym-ptrs)
   (substance-meronym-ptrs :accessor substance-meronym-ptrs :initform nil :initarg :substance-meronym-ptrs)
   (part-meronym-ptrs :accessor part-meronym-ptrs :initform nil :initarg :part-meronym-ptrs)
   (member-holonym-ptrs :accessor member-holonym-ptrs :initform nil :initarg :member-holonym-ptrs)
   (substance-holonym-ptrs :accessor substance-holonym-ptrs :initform nil :initarg :substance-holonym-ptrs)
   (part-holonym-ptrs :accessor part-holonym-ptrs :initform nil :initarg :part-holonym-ptrs)
   (morphology-ptrs :accessor morphology-ptrs :initform nil :initarg :morphology-ptrs)
   (class-ptrs :accessor class-ptrs :initform nil :initarg :class-ptrs)
   (cause-ptrs :accessor cause-ptrs :initform nil :initarg :cause-ptrs)
   ;; still need grouped-verbs, attributes, antonyms, see-also, participles,
   ;; pertainyms, frames, adj-noun

   ;; Aggregations
   (semantic-neighborhood-ptrs :accessor semantic-neighborhood-ptrs :initform nil
                               :initarg :semantic-neighborhood-ptrs)
   (meronym-ptrs :accessor meronym-ptrs :initform nil :initarg :meronym-ptrs)
   (holonym-ptrs :accessor holonym-ptrs :initform nil :initarg :holonym-ptrs)
   (semantic-parent-ptrs :accessor semantic-parent-ptrs :initform nil :initarg :semantic-parent-ptrs)
   (semantic-child-ptrs :accessor semantic-child-ptrs :initform nil :initarg :semantic-child-ptrs)

   (syntactic-markers :accessor syntactic-markers
                      :initform (make-array '(0)
                                            :adjustable t
                                            :element-type 'string)
                      :initarg :syntactic-markers)

   (synset-words :accessor synset-words
                 :initform (make-array '(0)
                                       :adjustable t
                                       :element-type 'string)
                 :initarg :synset-words)))

(defgeneric synset-p (synset)
  (:method ((synset synset)) t)
  (:method (thing) nil))

(defmethod print-object ((synset synset) stream)
  (format stream "#<SYNSET ~A: ~S>"
          (synset-id synset)
          ;;(sense-number synset)
          (synset-words synset)))

(defmethod synset-eq ((synset1 synset) (synset2 synset))
  (= (synset-id synset1) (synset-id synset2)))

(defmethod lookup-synset ((language language) synset-id)
  (gethash synset-id (synset-table language)))

(defmethod synsets ((language language) word &key pos)
  (cond ((null pos)
         (gethash word (word-to-synset-table language)))
        ((symbolp pos)
         (gethash (cons word (wordnet-pos pos))
                  (word-pos-to-synset-table language)))
        ((integerp pos)
         (gethash (cons word pos)
                  (word-pos-to-synset-table language)))))

;;(p)    predicate position
;;(a)    prenominal (attributive) position
;;(ip)    immediately postnominal position
(defmethod syntax ((synset synset) (word string))
  (let ((position (position word (synset-words synset) :test 'equal)))
    (when position
      (syntax synset (1+ position)))))

(defmethod syntax ((synset synset) (position integer))
  (handler-case
      (elt (syntactic-markers synset) position)
    (error (c)
      (declare (ignore c))
      nil)))

(defun synset-type-to-wordnet-pos (synset-type)
  (cond ((equal synset-type "n") +noun+)
        ((equal synset-type "v") +verb+)
        ((equal synset-type "a") +adjective+)
        ((equal synset-type "s") +satellite+)
        ((equal synset-type "r") +adverb+)))

(defun synset-type-to-pos (synset-type)
  (cond ((equal synset-type "n") :NN) ;;+noun-tag+)
        ((equal synset-type "v") :VB) ;;+verb-tag+)
        ((equal synset-type "a") :JJ) ;;+adjective-tag+)
        ((equal synset-type "s") :JJ) ;;+adjective-tag+)
        ((equal synset-type "r") :RB))) ;;+adverb-tag+)))

(defun wordnet-pos (tag)
  "Translate a PennTreebank POS tag into a Wordnet POS id"
  (case tag
    ((+noun-tag+ :NN :NNS :NNP :NNPS)     cffi-wordnet:+noun+)
    ((+verb-tag+ :VB :VBD :VBG :VBN :VBZ) cffi-wordnet:+verb+)
    ((+adjective-tag+ :JJ :JJR :JJS)      cffi-wordnet:+adjective+)
    ((+adverb-tag+ :RB :RBR :RBS)         cffi-wordnet:+adverb+)
    (otherwise            nil)))

(defmethod make-synset ((language language) &key synset-id synset-type sense-number
                                              tag-count)
  (assert (member synset-type '("n" "v" "a" "s" "r") :test 'string=))
  (let ((synset (make-instance 'synset
                               :synset-id synset-id
                               :synset-type synset-type
                               :sense-number sense-number
                               :tag-count tag-count)))
    (setf (gethash synset-id (synset-table language)) synset)))

(defmethod add-word-to-synset ((language language) (synset synset) (word string)
                               (word-number integer))
  (unless (>= (array-dimension (synset-words synset) 0)
              word-number)
    (adjust-array (synset-words synset) (list word-number)))
  (setf (aref (synset-words synset) (1- word-number)) word)
  (pushnew synset
           (gethash word (word-to-synset-table language))
           :test 'synset-eq)
  (pushnew synset (gethash (list word
                                 (synset-type-to-wordnet-pos
                                  (synset-type synset)))
                           (word-pos-to-synset-table language))
           :test 'synset-eq)
  (pushnew synset (gethash (list word (sense-number synset))
                           (word-sense-to-synset-table language))
           :test 'synset-eq)
  synset)

(defmethod read-wordnet-synsets ((language language) &key (path "prolog")
                                                       (file "wn_s.pl"))
  (with-open-file (in (format nil "~A/~A" path file))
    (clrhash (synset-table language))
    (clrhash (word-to-synset-table language))
    (clrhash (word-sense-to-synset-table language))
    (clrhash (word-pos-to-synset-table language))
    (let ((count 0))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
	(let* ((length (length line))
	       (end (- length 2)))
	  (when (> length 0)
	    (assert (string= (subseq line 0 2) "s("))
	    (assert (string= (subseq line end length) ")."))
	    (destructuring-bind (id word-number quoted-word ss-type
				    &optional sense-number tag-count)
		(split-sequence:split-sequence #\, (subseq line 2 end))
	      (assert (find ss-type '("n" "v" "a" "s" "r") :test #'string=))
	      (incf count)
	      (let ((word (subseq quoted-word 1 (1- (length quoted-word))))
		    (word-number (parse-integer word-number))
		    (synset-id (parse-integer id)))
		(let ((synset (or (lookup-synset language synset-id)
				  (make-synset language
					       :synset-id synset-id
					       :synset-type ss-type
					       :sense-number (when sense-number
							       (parse-integer
								sense-number))
					       :tag-count (when tag-count
							    (parse-integer
							     tag-count))))))
		  (add-word-to-synset language synset word word-number)))))))
      count)))

(defmethod read-glosses ((language language) &key (path "prolog") (file "wn_g.pl"))
  (with-open-file (in (format nil "~A/~A" path file))
    (let ((count 0))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
	(let* ((length (length line))
	       (end (- length 2)))
	  (when (> length 0)
	    (assert (string= (subseq line 0 2) "g("))
	    (assert (string= (subseq line end length) ")."))
	    (destructuring-bind (ss-id quoted-gloss)
		(split-sequence:split-sequence #\, (subseq line 2 end) :count 2)
	      (incf count)
	      (let ((gloss (subseq quoted-gloss 1 (1- (length quoted-gloss))))
		    (synset-id (parse-integer ss-id)))
		(let ((synset (lookup-synset language synset-id)))
		  (if (not (synset-p synset))
		      (log:info "Line ~A: Invalid synset id ~A" count synset-id)
		      (setf (synset-gloss synset) gloss))))))))
      count)))


(defmethod read-syntactic-markers ((language language) &key (path "prolog")
                                                         (file "wn_syntax.pl"))
  (with-open-file (in (format nil "~A/~A" path file))
    (let ((count 0))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (when (> (length line) 0)
	  (let* ((length (length line))
		 (end (- length 2)))
	    (assert (string= (subseq line 0 7) "syntax("))
	    (assert (string= (subseq line end length) ")."))
	    (destructuring-bind (ss-id word-number marker)
		(split-sequence:split-sequence #\, (subseq line 7 end) :count 3)
	      (incf count)
	      (let ((synset-id (parse-integer ss-id)))
		(let ((synset (lookup-synset language synset-id)))
		  (if (not (synset-p synset))
		      (log:info "Line ~A: Invalid synset id ~A" count synset-id)
		      (let ((position (parse-integer word-number)))
			(unless (= (length (syntactic-markers synset))
				   (length (synset-words synset)))
			  (setf (syntactic-markers synset)
				(make-array (list (length (synset-words synset))))))
			(setf (elt (syntactic-markers synset) (1- position))
			      marker)))))))))
      count)))

(defmethod read-wordnet-pointerfile ((language language) file prolog-op slot-name
                                     &key reverse-links)
  (with-open-file (in file)
    (let ((count 0)
	  (op-length (length prolog-op)))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (when (> (length line) 0)
	  (let* ((length (length line))
		 (end (- length 2)))
	    (assert (string= (subseq line 0 op-length) prolog-op))
	    (assert (string= (subseq line op-length (1+ op-length)) "("))
	    (assert (string= (subseq line end length) ")."))
	    (destructuring-bind (ss-id-1 ss-id-2)
		(split-sequence:split-sequence #\, (subseq line (1+ op-length) end)
					       :count 2)
	      (incf count)
	      (let ((synset-id-1 (parse-integer ss-id-1))
		    (synset-id-2 (parse-integer ss-id-2)))
		(let ((synset-1 (lookup-synset language synset-id-1))
		      (synset-2 (lookup-synset language synset-id-2)))
		  (when (not (synset-p synset-1))
		    (error "Line ~A: Invalid synset id ~A" count synset-id-1))
		  (when (not (synset-p synset-2))
		    (error "Line ~A: Invalid synset id ~A" count synset-id-2))
		  (if reverse-links
		      (pushnew synset-1 (slot-value synset-2 slot-name))
		      (pushnew synset-2 (slot-value synset-1 slot-name)))))))))
      count)))

(defmethod synset-word-list ((synset synset))
  (map 'list 'identity (synset-words synset)))

(defmethod convert-pos-to-wordnet (pos)
  (typecase pos
    (symbol (wordnet-pos pos))
    (integer pos)
    (otherwise (error "Unknown part of speech: ~A" pos))))

(defmethod synonyms (word &key (language *language*) pos (return-type :synsets))
  (let ((synsets
         (if pos
             (gethash (list word (convert-pos-to-wordnet pos))
                      (word-pos-to-synset-table language))
             (gethash word (word-to-synset-table language)))))
    (if (eql return-type :words)
        (delete-duplicates (mapcan 'synset-word-list synsets) :test 'string=)
        synsets)))

(defmethod synonym-p ((word1 string) (word2 string) &key pos (language *language*))
  (intersection (synonyms word1 :return-type :words :pos pos :language language)
                (synonyms word2 :return-type :words :pos pos :language language)))

(defmethod glosses ((word string) &key pos (language *language*))
  (map 'list 'nlp::synset-gloss (synonyms word :pos pos :language language)))

(defmethod hypernyms ((synset synset) &key &allow-other-keys)
  (map 'list 'identity (hypernym-ptrs synset)))

(defmethod hypernyms ((word string) &key pos (language *language*)
                                      &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'hypernym-ptrs synsets))))

(defmethod hyponyms ((synset synset) &key &allow-other-keys)
  (map 'list 'identity (hyponym-ptrs synset)))

(defmethod hyponyms ((word string) &key pos (language *language*)
                                     &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'hyponym-ptrs synsets))))

(defmethod make-meronyms ((synset synset) &key &allow-other-keys)
  (union (union (map 'list 'identity (member-meronym-ptrs synset))
                (map 'list 'identity (substance-meronym-ptrs synset)))
         (map 'list 'identity (part-meronym-ptrs synset))))

(defmethod meronyms ((synset synset) &key &allow-other-keys)
  (meronym-ptrs synset))

(defmethod meronyms ((word string) &key pos (language *language*)
                                     &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'meronym-ptrs synsets))))

(defmethod make-holonyms ((synset synset) &key &allow-other-keys)
  (union (union (map 'list 'identity (member-holonym-ptrs synset))
                (map 'list 'identity (substance-holonym-ptrs synset)))
         (map 'list 'identity (part-holonym-ptrs synset))))

(defmethod holonyms ((synset synset) &key &allow-other-keys)
  (holonym-ptrs synset))

(defmethod holonyms ((word string) &key pos (language *language*)
                                     &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'holonym-ptrs synsets))))

(defun instances (word &key pos (language *language*))
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'instance-ptrs synsets))))

(defmethod semantic-neighborhood ((word string) &key pos (language *language*))
  (union-all (mapcar 'semantic-neighborhood-ptrs
                     (synonyms word :pos pos :language language))))

(defmethod semantic-neighborhood ((synset synset) &key &allow-other-keys)
  (semantic-neighborhood-ptrs synset))

(defmethod make-semantic-neighborhood ((synset synset) &key (language *language*)
                                                         &allow-other-keys)
  (union-all (list (instances synset :language language)
                   (meronyms synset :language language)
                   (holonyms synset :language language)
                   (hyponyms synset :language language)
                   (hypernyms synset :language language))))

(defmethod make-semantic-parents ((synset synset) &key &allow-other-keys)
  (union (holonyms synset)
         (hypernyms synset)))

(defmethod semantic-parents ((word string) &key pos (language *language*)
                                             &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'semantic-parent-ptrs synsets))))

(defmethod semantic-parents ((synset synset) &key &allow-other-keys)
  (semantic-parent-ptrs synset))

(defmethod make-semantic-children ((synset synset) &key &allow-other-keys)
  (union (meronyms synset)
         (hyponyms synset)))

(defmethod semantic-children ((word string) &key pos (language *language*)
                                              &allow-other-keys)
  (let ((synsets (if pos
                     (gethash (list word (convert-pos-to-wordnet pos))
                              (word-pos-to-synset-table language))
                     (gethash word (word-to-synset-table language)))))
    (union-all (mapcar 'semantic-child-ptrs synsets))))

(defmethod semantic-children ((synset synset) &key &allow-other-keys)
  (semantic-child-ptrs synset))

(defmethod populate-wordnet-database ((language language) &key (path "prolog"))
  (read-wordnet-synsets language :path path)
  (read-glosses language :path path)
  (read-syntactic-markers language :path path)
  (read-wordnet-pointerfile language (format nil "~A/wn_hyp.pl" path)
                            "hyp" 'hypernym-ptrs)
  (read-wordnet-pointerfile language (format nil "~A/wn_hyp.pl" path)
                            "hyp" 'hyponym-ptrs
                            :reverse-links t)
  (read-wordnet-pointerfile language (format nil "~A/wn_ins.pl" path)
                            "ins" 'instance-ptrs)
  (read-wordnet-pointerfile language (format nil "~A/wn_mm.pl" path)
                            "mm" 'member-meronym-ptrs
                            :reverse-links t)
  (read-wordnet-pointerfile language (format nil "~A/wn_ms.pl" path)
                            "ms" 'substance-meronym-ptrs
                            :reverse-links t)
  (read-wordnet-pointerfile language (format nil "~A/wn_mp.pl" path)
                            "mp" 'part-meronym-ptrs
                            :reverse-links t)
  (read-wordnet-pointerfile language (format nil "~A/wn_mm.pl" path)
                            "mm" 'member-holonym-ptrs)
  (read-wordnet-pointerfile language (format nil "~A/wn_ms.pl" path)
                            "ms" 'substance-holonym-ptrs)
  (read-wordnet-pointerfile language (format nil "~A/wn_mp.pl" path)
                            "mp" 'part-holonym-ptrs)
  ;; Calculate important aggregations
  (maphash (lambda (id synset)
             (declare (ignore id))
             (setf (semantic-neighborhood-ptrs synset)
                   (make-semantic-neighborhood synset :language language)
                   (meronym-ptrs synset)
                   (make-meronyms synset)
                   (holonym-ptrs synset)
                   (make-holonyms synset)
                   (semantic-parent-ptrs synset)
                   (make-semantic-parents synset)
                   (semantic-child-ptrs synset)
                   (make-semantic-children synset)))
           (synset-table language))
  (synset-table language))
