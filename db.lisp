(in-package :nlp)

(defvar *languages* (make-hash-table :test 'equalp))

(defclass language ()
  ((name :accessor name :initform "" :initarg :name)
   (aliases :accessor aliases :initform nil :initarg :aliases)
   (default-encoding :accessor default-encoding :initform nil
                     :initarg :default-encoding)
   (alphabet :accessor alphabet :initform nil :initarg :alphabet)

   ;; POS tagging
   (total-count :accessor total-count :initform 0)
   (gammas :accessor gammas :initform (make-hash-table))
   (probabilities :accessor probabilities :initform (make-hash-table :test 'equal))
   (unigrams :accessor unigrams :initform (make-hash-table :test 'equal))
   (bigrams :accessor bigrams :initform (make-hash-table :test 'equal))
   (trigrams :accessor trigrams :initform (make-hash-table :test 'equal))
   (tag-occurrences :accessor tag-occurrences
                    :initform (make-hash-table :test 'equal))
   (word-occurrences :accessor word-occurrences
                     :initform (make-hash-table :test 'equalp
                                                :weakness :key-or-value))
   (word-occurrence-file :accessor word-occurrence-file :initform ""
                         :initarg :word-occurrence-file)
   (word-occurrence-dbm :accessor word-occurrence-dbm :initform nil
                        :initarg :word-occurrence-dbm)

   (observations :accessor observations
                 :initform (make-hash-table :test 'equal
                                            :weakness :key-or-value))
   (observations-file :accessor observations-file :initform ""
                         :initarg :observations-file)
   (observations-dbm :accessor observations-dbm :initform nil
                        :initarg :observations-dbm)

   (unknown-probability :accessor unknown-probability :initform 0)

   ;; HMM Chunking
   (np-total-count :accessor np-total-count :initform 0)
   (np-gammas :accessor np-gammas :initform (make-hash-table))
   (np-probabilities :accessor np-probabilities
                     :initform (make-hash-table :test 'equal))
   (np-unigrams :accessor np-unigrams :initform (make-hash-table :test 'equal))
   (np-bigrams :accessor np-bigrams :initform (make-hash-table :test 'equal))
   (np-trigrams :accessor np-trigrams :initform (make-hash-table :test 'equal))
   (np-tag-occurrences :accessor np-tag-occurrences
                       :initform (make-hash-table :test 'equal))
   (np-pos-occurrences :accessor np-pos-occurrences
                       :initform (make-hash-table :test 'equal))
   (np-observations :accessor np-observations
                    :initform (make-hash-table :test 'equal))
   (np-unknown-probability :accessor np-unknown-probability :initform 0)

   ;; Lexicon
   (lexicon-file :accessor lexicon-file :initform "" :initarg :lexicon-file)
   (lexicon-dbm :accessor lexicon-dbm :initform nil :initarg :lexicon-dbm)
   (plexicon-file :accessor plexicon-file :initform "" :initarg :plexicon-file)
   (plexicon-dbm :accessor plexicon-dbm :initform nil :initarg :plexicon-dbm)

   (vowels :accessor vowels :initform nil :initarg :vowels)

   (lexicon :accessor lexicon :initform (make-hash-table :test 'equal))
   (plexicon :accessor plexicon :initform (make-hash-table :test 'equal))
   (word-freq :accessor word-freq :initform (make-hash-table :test 'equal))

   (user-pos-regex :accessor user-pos-regex :initform nil)
   (contraction-table :accessor contraction-table
                      :initform (make-hash-table :test 'equalp))
   (stop-words :accessor stop-words :initform (make-hash-table :test 'equalp))
   (ngrams :accessor ngrams :initform (make-hash-table :test 'equal))

   ;; Wordnet
   (synset-table :accessor synset-table :initform (make-hash-table))
   (word-to-synset-table :accessor word-to-synset-table
                         :initform (make-hash-table :test 'equalp))
   (word-sense-to-synset-table :accessor word-sense-to-synset-table
                               :initform (make-hash-table :test 'equalp))
   (word-pos-to-synset-table :accessor word-pos-to-synset-table
                             :initform (make-hash-table :test 'equalp))

   ;; Parsing
   (np-regexes :accessor np-regexes :initform nil)
   (cfg :accessor cfg :initform (make-hash-table))
   (cfg-idx :accessor cfg-idx :initform (make-hash-table))
   (pcfg :accessor pcfg :initform (make-hash-table :test 'equalp))
   (lcfg :accessor lcfg :initform nil) ;;(make-lcfg-table))
   (cnf-grammar :accessor cnf-grammar :initform (make-hash-table))
   (cnf-index :accessor cnf-index :initform (make-hash-table :test 'equalp))
   (cnf-subs-map :accessor cnf-subs-map
                 :initform (make-hash-table :test 'equalp))
   (cnf-subs-rev :accessor cnf-subs-rev
                 :initform (make-hash-table :test 'equalp))))
