(in-package #:cffi-wordnet)

(defparameter *wordnet-lock* (sb-thread:make-mutex :name "wordnet-lock"))

(defun wordnet-init ()
  (wninit))

(defun morph-word (word pos)
  (if (and word pos)
      (sb-thread:with-recursive-lock (*wordnet-lock*)
        (or (morphword word pos) word))
      word))

(defun get-synset-words (synset)
  (let ((result nil))
    (dotimes (i (cffi:foreign-slot-value synset '(:struct Synset) 'wcount))
      (push
       (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'words) :string i)
       result))
    (mapcar (lambda (word)
              (cl-ppcre:regex-replace-all "_" (string-downcase word) " "))
            (nreverse result))))

(defun get-synset-definition (synset)
  (cffi:foreign-slot-value synset '(:struct Synset) 'defn))

(defun synonyms (word &key (part-of-speech +noun+) (sense +all-senses+)
                        (search-type +synonyms+))
  (when part-of-speech
    (sb-thread:with-recursive-lock (*wordnet-lock*)
      (let ((synset (findtheinfo_ds word part-of-speech search-type sense))
            (words nil))
        (unwind-protect
             (let ((ss synset))
               (loop
                  (if (and (pointerp ss) (null-pointer-p ss))
                      (return)
                      (progn
                        (dolist (word (get-synset-words ss))
                          (pushnew word words :test 'string=))
                        (setq ss (cffi:foreign-slot-value ss '(:struct Synset) 'nextss))))))
          (free_syns synset))
        words))))

(defclass synset-vertex ()
  ((word-list :accessor word-list :initform nil :initarg :word-list)))

(defmethod print-object ((v synset-vertex) stream)
  (format stream "(~{~A~^, ~})" (word-list v)))

(defmethod make-synset-vertex (&key word-list)
  (make-instance 'synset-vertex :word-list word-list))

(defun walk-up-hypernym-tree (synset graph root &key (spaces 0) (edge-type :has-hypernym))
  (let ((pointer-count (cffi:foreign-slot-value synset '(:struct Synset) 'ptrcount)))
    ;;(dotimes (i spaces) (format t " "))
    ;;(format t "~S~%" (get-synset-words synset))
    (let* ((words (get-synset-words synset))
           (vertex (add-node graph (make-synset-vertex :word-list words))))
      (unless (edge-exists? graph root vertex :edge-type edge-type)
        (add-edge graph root vertex :edge-type edge-type))
      (dotimes (i pointer-count)
        (handler-case
            (let ((this-ptrtype (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptrtyp) :int i))
                  (this-ppos (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ppos) :int i))
                  (this-ptroff (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptroff) :long i)))
              (when (or (eq this-ptrtype HYPERPTR)
                        (eq this-ptrtype INSTANCE))
                (let ((synset1 (read_synset this-ppos this-ptroff "")))
                  (unwind-protect
                       (walk-up-hypernym-tree synset1 graph vertex :spaces (+ 2 spaces) :edge-type edge-type)
                  (free_syns synset1)))))
        (error (e)
          (dotimes (i spaces) (format t " "))
          (format t "GOT ERROR '~A'~%~%" e)))))))

(defun hypernym-graph (word &key (part-of-speech +noun+) (sense +all-senses+)
                              graph root (edge-type :has-hypernym))
  (when part-of-speech
    (sb-thread:with-recursive-lock (*wordnet-lock*)
      (let ((synset (findtheinfo_ds (or (morph-word word part-of-speech) word)
                                    part-of-speech
                                    +hypernym+
                                    sense)))
        (unless graph
          (setq graph (make-typed-graph :node-comparator #'equalp)))
        (unless root
          (setq root (add-node graph word)))
        (unwind-protect
             (loop until (or (null synset)
                             (and (pointerp synset) (null-pointer-p synset))) do
                  (walk-up-hypernym-tree synset graph root :edge-type edge-type)
                  (let ((next-synset (cffi:foreign-slot-value synset '(:struct Synset) 'nextss)))
                    ;;(free_syns synset)
                    (setq synset next-synset)))
          (free_syns synset))))
    (values graph root)))

(defun hypernyms (word &key (part-of-speech +noun+) (sense +all-senses+))
  (when part-of-speech
    (sb-thread:with-recursive-lock (*wordnet-lock*)
      (let ((synset (findtheinfo_ds (or (morph-word word part-of-speech) word)
                                    part-of-speech
                                    +hypernym+
                                    sense))
            (hypernyms nil))
        (unwind-protect
             (loop until (or (null synset)
                             (and (pointerp synset) (null-pointer-p synset))) do
                  (let ((pointer-count (cffi:foreign-slot-value synset '(:struct Synset) 'ptrcount)))
                    (dotimes (i pointer-count)
                      (handler-case
                          (let ((this-ptrtype (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptrtyp) :int i))
                                (this-ppos (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ppos) :int i))
                                (this-ptroff (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptroff) :long i)))
                            (when (or (eq this-ptrtype HYPERPTR)
                                      (eq this-ptrtype INSTANCE))
                              (let ((synset1 (read_synset this-ppos this-ptroff "")))
                                (unwind-protect
                                     (dolist (word (get-synset-words synset1))
                                       (pushnew word hypernyms :test 'string=))
                                  (free_syns synset1)))))
                        (error (e)
                          (format t "GOT ERROR '~A'~%~%" e)))))
                  (let ((next-synset (cffi:foreign-slot-value synset '(:struct Synset) 'nextss)))
                    (setq synset next-synset)))
          (free_syns synset))
        hypernyms))))

(defun walk-up-holonym-tree (synset graph root &key (edge-type :has-holonym) (spaces 0))
  (let ((pointer-count (cffi:foreign-slot-value synset '(:struct Synset) 'ptrcount)))
    ;;(dotimes (i spaces) (format t " "))
    ;;(format t "~S~%" (get-synset-words synset))
    (let* ((words (get-synset-words synset))
           (vertex (add-node graph (make-synset-vertex :word-list words))))
      (unless (edge-exists? graph root vertex :edge-type edge-type)
        (add-edge graph root vertex :edge-type edge-type))
      (dotimes (i pointer-count)
        (handler-case
            (let ((this-ptrtype (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptrtyp) :int i))
                  (this-ppos (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ppos) :int i))
                  (this-ptroff (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptroff) :long i)))
              (when (or (eq this-ptrtype ISPARTPTR)
                        (eq this-ptrtype ISMEMBERPTR)
                        (eq this-ptrtype ISSTUFFPTR))
                (let ((synset1 (read_synset this-ppos this-ptroff "")))
                  (unwind-protect
                       (walk-up-holonym-tree synset1 graph vertex
                                             :edge-type edge-type
                                             :spaces (+ 2 spaces))
                  (free_syns synset1)))))
        (error (e)
          (dotimes (i spaces) (format t " "))
          (format t "GOT ERROR '~A'~%~%" e)))))))

(defun holonym-graph (word &key (part-of-speech +noun+) (sense +all-senses+)
                             graph root (edge-type :has-holonym))
  (when part-of-speech
    (sb-thread:with-recursive-lock (*wordnet-lock*)
      (let ((synset (findtheinfo_ds (or (morph-word word part-of-speech) word)
                                    part-of-speech
                                    +holonym+
                                    sense)))
        (unless graph
          (setq graph (make-typed-graph :node-comparator #'equalp)))
        (unless root
          (setq root (add-node graph word)))
        (unwind-protect
             (loop until (or (null synset)
                             (and (pointerp synset) (null-pointer-p synset))) do
                  (walk-up-holonym-tree synset graph root :edge-type edge-type)
                  (let ((next-synset (cffi:foreign-slot-value synset '(:struct Synset) 'nextss)))
                    ;;(free_syns synset)
                    (setq synset next-synset)))
          (free_syns synset))))
    (values graph root)))

(defun holonyms (word &key (part-of-speech +noun+) (sense +all-senses+))
  (when part-of-speech
    (sb-thread:with-recursive-lock (*wordnet-lock*)
      (let ((synset (findtheinfo_ds (or (morph-word word part-of-speech) word)
                                    part-of-speech
                                    +holonym+
                                    sense))
            (holonyms nil))
        (unwind-protect
             (loop until (or (null synset)
                             (and (pointerp synset) (null-pointer-p synset))) do
                  (let ((pointer-count (cffi:foreign-slot-value synset '(:struct Synset) 'ptrcount)))
                    (dotimes (i pointer-count)
                      (handler-case
                          (let ((this-ptrtype (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptrtyp) :int i))
                                (this-ppos (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ppos) :int i))
                                (this-ptroff (cffi:mem-aref (cffi:foreign-slot-value synset '(:struct Synset) 'ptroff) :long i)))
                            (when (or (eq this-ptrtype ISPARTPTR)
                                      (eq this-ptrtype ISMEMBERPTR)
                                      (eq this-ptrtype ISSTUFFPTR))
                              (let ((synset1 (read_synset this-ppos this-ptroff "")))
                                (unwind-protect
                                     (dolist (word (get-synset-words synset1))
                                       (pushnew word holonyms :test 'string=))
                                  (free_syns synset1)))))
                        (error (e)
                          (format t "GOT ERROR '~A'~%~%" e)))))
                  (let ((next-synset (cffi:foreign-slot-value synset '(:struct Synset) 'nextss)))
                    (setq synset next-synset)))
          (free_syns synset))
        holonyms))))

(defun test ()
  (wninit)
  (format t "~A~%" (findtheinfo "block" VERB SYNS ALLSENSES))
  (let ((synset (findtheinfo_ds "block" VERB MERONYM ALLSENSES)))
    (unwind-protect
	 (progn
	   (format t "Sense 1, words: ~A~%"
		   (cffi:foreign-slot-value synset '(:struct Synset) 'wcount))
	   (get-synset-words synset))
      (free_syns synset))))
