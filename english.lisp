(in-package :nlp)

(defclass english (language)
  ((name :accessor name :initform "english" :initarg :name)
   (aliases :accessor aliases
            :initform '("en" "en-us" "en-gb" "en-ca" "en-au" "en-bz" "en-cb"
                        "en-in" "en-ie" "en-jm" "en-nz" "en-ph" "en-za" "en-tt")
            :initarg :aliases)
   (vowels :accessor vowels :initform '(#\a #\e #\i #\o #\u #\y)
           :initarg :vowels)
   (default-encoding :accessor default-encoding :initform :utf-8
                     :initarg :default-encoding)))

(defmethod noun-p ((language english) (symbol symbol))
  (member symbol '(:NN :NNS :NNP :NNPS)))

(defmethod adjective-p ((language english) (symbol symbol))
  (member symbol '(:JJ :JJR :JJS)))

(defmethod verb-p ((language english) (symbol symbol))
  (member symbol '(:VB :VBD :VBG :VBN :VBZ :VBP)))

(defmethod adverb-p ((language english) (symbol symbol))
  (member symbol '(:RB :RBR :RBS :WRB)))

(defmethod determiner-p ((language english) (symbol symbol))
  (member symbol '(:DT :PDT)))

(defmethod pronoun-p ((language english) (symbol symbol))
  (member symbol '(:PRP :PRP$ :WP :WP$)))


(def-pos-map :english
    (("!" ".")
     ("#" ".")
     ("$" ".")
     ("''" ".")
     ("(" ".")
     (")" ".")
     ("," ".")
     ("-LRB-" ".")
     ("-RRB-" ".")
     ("." ".")
     (":" ".")
     ("?" ".")
     ("CC" "CONJ")
     ("CD" "NUM")
     ("CD|RB" "X")
     ("DT" "DET")
     ("EX" "DET")
     ("FW" "X")
     ("IN" "ADP")
     ("IN|RP" "ADP")
     ("JJ" "ADJ")
     ("JJR" "ADJ")
     ("JJRJR" "ADJ")
     ("JJS" "ADJ")
     ("JJSS" "ADJ")
     ("JJ|RB" "ADJ")
     ("JJ|VBG" "ADJ")
     ("LS" "X")
     ("MD" "VERB")
     ("NN" "NOUN")
     ("NNP" "NOUN")
     ("NNPS" "NOUN")
     ("NPS" "NOUN")
     ("NNS" "NOUN")
     ("NN|NNS" "NOUN")
     ("NN|SYM" "NOUN")
     ("NN|VBG" "NOUN")
     ("NP" "NOUN")
     ("PDT" "DET")
     ("POS" "PRT")
     ("PRP" "PRON")
     ("PRP$" "PRON")
     ("PRP$R" "PRON")
     ("PRP|VBP" "PRON")
     ("PRT" "PRT")
     ("RB" "ADV")
     ("RBR" "ADV")
     ("RBS" "ADV")
     ("RB|RP" "ADV")
     ("RB|VBG" "ADV")
     ("RN" "X")
     ("RP" "PRT")
     ("SYM" "X")
     ("TO" "PRT")
     ("UH" "X")
     ("VB" "VERB")
     ("VBD" "VERB")
     ("VBD|VBN" "VERB")
     ("VBG" "VERB")
     ("VBG|NN" "VERB")
     ("VBN" "VERB")
     ("VBP" "VERB")
     ("VBP|TO" "VERB")
     ("VBZ" "VERB")
     ("VP" "VERB")
     ("WDT" "DET")
     ("WH" "X")
     ("WP" "PRON")
     ("WP$" "PRON")
     ("WRB" "ADV")
     ("``" ".")))

(defmethod stem ((language english) word &key &allow-other-keys)
  (porter-stemmer:stem word))

(defmethod lookup-generic-pos ((name (eql :english)) pos &key &allow-other-keys)
  (let ((alist (gethash name *pos-maps*)))
    (when alist
      (cdr (assoc pos alist
                  :test (lambda (key1 key2)
                          (equalp key2 key1)))))))

(defun generify-english-pos-corpus (&key (file "all-pos.txt"))
  (with-open-file (stream "data/english-pos.txt"
                          :direction :output
                          :element-type 'character
                          :external-format :utf-8
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-open-file (in-stream file
                               :direction :input
                               :element-type 'character
                               :external-format :utf-8)
      (do ((sentence (read-line in-stream nil :eof)
                     (read-line in-stream nil :eof)))
          ((eql :eof sentence))
        (let ((pairs (split "\\s+" sentence)))
          (dotimes (i (length pairs))
            (multiple-value-bind (word pos)
                (tagged-split (nth i pairs))
              (let ((generic-pos (lookup-generic-pos :english (symbol-name pos))))
                (unless generic-pos
                  (error "Unknown POS: ~A" pos))
                (format stream "~A/~A" word generic-pos)
                (unless (= i (1- (length sentence)))
                  (format stream " "))))))
        (terpri stream)))))

(defun flatten-english-phrase-tree (tree)
  (let ((leaf-phrases nil))
    (labels ((dfs-helper (this-node phrase-type)
               (cond ((and (consp this-node)
                           (atom (first this-node))
                           (atom (second this-node)))
                      (let ((pos (symbol-name (first this-node))))
                        (unless (equalp pos "-NONE-")
                          (let ((generic-pos
                                 ;; Leaving generic POS tags behind for now
                                 ;;(lookup-generic-pos :english pos)))
                                 (intern (string-upcase pos) :keyword)))
                            (unless generic-pos
                              (error "Unknown POS: '~A'" pos))
                            (push (list phrase-type generic-pos) leaf-phrases)))))
                     ((and (consp this-node)
                           (atom (first this-node))
                           (consp (second this-node)))
                      (dolist (phrase (rest this-node))
                        (dfs-helper phrase (first this-node)))))))
      (handler-case
          (dfs-helper tree nil)
        (error (c)
          (log:error "~A" c)
          nil)
        (:no-error (rv)
          (declare (ignore rv))
          (nreverse leaf-phrases))))))

(defmethod train-phrase-extractor ((language english) file)
  "Train the noun phrase extractor on a given english labeled corpus."
  (reset-chunker-tables language)
  (map-sexp-corpus
   (lambda (tree)
     (learn-phrase-pattern language (flatten-english-phrase-tree tree)))
   file)
  (compute-np-observation-likelihoods language)
  (compute-np-ngram-probabilities language)
  language)

(defmethod load-contraction-table ((language english)
                                   &optional (file "data/contractions.txt"))
  (with-open-file (in file)
    (setf (contraction-table language) (make-hash-table :test 'equalp))
    (do ((line (read-line in nil :eof) (read-line in nil :eof)))
        ((eql line :eof))
      (multiple-value-bind (match-p matches)
          (scan-to-strings "^([a-zA-Z']+)\\s+(.*)$" line)
        (when match-p
          (setf (gethash (elt matches 0) (contraction-table language))
                (split "\\s+" (elt matches 1))))))))

(defun make-english-db (&key
                          (wordnet-path "prolog")
                          grammar-train
                          (grammar-load "data/p-grammar.txt")
                          (pos-train "data/english-pos.txt")
                          (pos-lex "data/english-pos.txt")
                          (moby-file "data/moby/mobypos.txt")
                          (contraction-file "data/english-contractions.txt")
                          user-lexicon-file
                          (stop-words-file "data/english-stop-words.txt")
                          (chunker-train "data/english-parsed.txt")
                          save-p profile-p)
  (flet ((build-it ()
           (let ((*language* (make-new-language 'english)))
             (setf (alphabet *language*)
                   "abcdefghijklmnopqrstuvwxyz0123456789-'/")
             (log:info "Training English NLP system...")
             (log:info "Building and training lexicon...")
             (load-contraction-table *language* contraction-file)
             (load-stop-words *language* stop-words-file)
             (populate-wordnet-database *language* :path wordnet-path)
             (maybe-profile
              (make-lexicon *language*
                            pos-lex
                            :moby-file moby-file
                            :user-file user-lexicon-file))
             (log:info "Training POS tagger...")
             (maybe-profile  (train-tagger *language* pos-train))
             (log:info "Training HMM Chunker...")
             (maybe-profile (train-phrase-extractor *language* chunker-train))
             (log:info "Training grammar parser...")
             (if grammar-load
                 (maybe-profile (load-pcfg grammar-load *language*))
                 (maybe-profile (extract-grammar-rules grammar-train *language*)))
             ;;(log:info "Converting CFG to CNF...")
             ;;(maybe-profile (make-cnf-grammar (pos-cfg *pos-db*) *pos-db*))
             (when save-p
               (log:info "Freezing POS database...")
               (maybe-profile (freeze-nlp *language*)))
             (add-language *language*)
             *language*)))
    (maybe-profile (build-it))))
