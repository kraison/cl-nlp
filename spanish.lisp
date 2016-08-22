(in-package :nlp)

(defclass spanish (language)
  ((name :accessor name :initform "spanish" :initarg :name)
   (aliases :accessor aliases
            :initform '("es" "es-ar" "es-bo" "es-cl" "es-co" "es-cr" "es-do"
                        "es-ec" "es-sv" "es-gt" "es-hn" "es-mx" "es-ni"
                        "es-pa" "es-py" "es-pe" "es-pr" "es-es""es-uy" "es-ve")
            :initarg :aliases)
   (vowels :accessor vowels :initform '(#\a #\e #\i #\o #\u #\á #\é #\í #\ó #\ú #\ü)
           :initarg :vowels)
   (default-encoding :accessor default-encoding :initform :latin-1
                     :initarg :default-encoding)))

(defmethod noun-p ((language spanish) symbol)
  (member symbol '(:NC :NP :W)))

(defmethod adjective-p ((language spanish) symbol)
  (member symbol '(:AO :AQ)))

(defmethod verb-p ((language spanish) symbol)
  (member symbol '(:VA :VM :VS)))

(defmethod adverb-p ((language spanish) symbol)
  (member symbol '(:RG :RN)))

(defmethod determiner-p ((language spanish) symbol)
  (member symbol '(:DA :DD :DE :DI :DN :DP :DT)))

(defmethod pronoun-p ((language spanish) symbol)
  (member symbol '(:P0 :PD :PE :PI :PN :PP :PR :PT :PX)))

(defun merge-spanish-corpora (&key
                                (ancora-dir "data/ancora-2.0/")
                                (wiki-dir "data/wiki-es/")
                                (cess-dir "data/cess_esp"))
  (with-open-file (stream "data/spanish-pos.txt"
                          :direction :output
                          :element-type 'character
                          :external-format :latin-1
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (map nil
         (lambda (sentence)
           (dotimes (i (length sentence))
             (format stream "~A/~A"
                     (cdr (nth i sentence))
                     (car (nth i sentence)))
             (unless (= i (1- (length sentence)))
               (format stream " ")))
           (terpri stream))
         (nconc
          (slurp-ancora-corpus ancora-dir)
          (read-cess-esp cess-dir)
          (read-wiki-es wiki-dir)))))

(let ((phrase-regex (create-scanner "^GRUP\.(NOM|VERB)" :case-insensitive-mode t)))
  (defun extract-spanish-phrase-patterns (tree)
    (let ((patterns nil))
      (labels ((extract-pattern (node)
                 (cond ((and (consp node)
                             (= (length node) 3)
                             (every 'atom node))
                        (lookup-specific-pos :wiki-es (symbol-name (first node))))
                       ((consp node)
                        (mapcar #'extract-pattern (rest node)))))
               (walk (node)
                 (when (consp node)
                   (when (and (symbolp (first node))
                              (scan phrase-regex (symbol-name (first node))))
                     (let ((pattern (flatten (extract-pattern node))))
                       (when (> (length pattern) 1)
                         (push (list (first node) pattern) patterns))))
                   (dolist (child node)
                     (walk child)))))
        (walk tree)
        (nreverse patterns)))))

(defun flatten-spanish-phrase-tree (tree)
  (let ((leaf-phrases nil))
    (labels ((dfs-helper (this-node phrase-type)
               (cond ((and (consp this-node)
                           (atom (first this-node))
                           (atom (second this-node))
                           (atom (third this-node)))
                      (push (list phrase-type
                                  (lookup-specific-pos
                                   :cess (symbol-name (first this-node))))
                            leaf-phrases))
                     ((and (consp this-node)
                           (atom (first this-node))
                           (consp (second this-node)))
                      (dolist (phrase (rest this-node))
                        (dfs-helper phrase (first this-node))))
                     ((consp this-node)
                      (dolist (child this-node)
                        (dfs-helper child (first this-node)))))))
      (dfs-helper tree nil))
    (nreverse leaf-phrases)))

(defmethod train-phrase-extractor ((language spanish) directory)
  "Train the noun phrase extractor on a given labeled corpus."
  (reset-chunker-tables language)
  (dolist (file (cl-fad:list-directory directory))
    (when (scan "sexp$" (namestring file))
      (handler-case
          (map-sexp-corpus
           (lambda (tree)
             (let ((flattened-tree (flatten-spanish-phrase-tree tree)))
               (learn-phrase-pattern language flattened-tree)))
           file
           :external-format :latin-1)
        (error (c)
          (log:error "ERROR PARSING ~A: ~A" file c)
          c))))
  (compute-np-observation-likelihoods language)
  (compute-np-ngram-probabilities language)
  language)

(defmethod load-contraction-table ((language spanish)
                                   &optional (file "data/spanish-contractions.txt"))
  (with-open-file (in file)
    (setf (contraction-table language) (make-hash-table :test 'equalp))
    (do ((line (read-line in nil :eof) (read-line in nil :eof)))
        ((eql line :eof))
      (multiple-value-bind (match-p matches)
          (scan-to-strings "^([a-zA-Z']+)\\s+(.*)$" line)
        (when match-p
          (setf (gethash (elt matches 0) (contraction-table language))
                (split "\\s+" (elt matches 1))))))))

(defun make-spanish-db (&key
                          profile-p
                          save-p
                          ;;(contraction-file "data/spanish-contractions.txt")
                          (user-lexicon "data/spanish-lexicon.txt")
                          (stop-words-file "data/spanish-stop-words.txt")
                          (pos-lex "data/spanish-pos-all.txt")
                          (pos-train "data/spanish-pos.txt")
                          (chunker-dir "data/cess_esp/"))
  (let ((*language* (make-new-language 'spanish)))
    (setf (alphabet *language*)
          "áéíóúñü¡abcdefghijklmnopqrstuvwxyz0123456789-'/")
    (log:info "Training Spanish NLP system...")
    (log:info "Building and training lexicon...")
    (load-stop-words *language* stop-words-file)
    ;; I am not convinced that splitting the 2 Spanish contractions provides
    ;; better results;  will test with cross validation.
    ;;(load-contraction-table *language* contraction-file)
    (maybe-profile
     (make-lexicon *language*
                   pos-lex
                   :user-file user-lexicon
                   :external-format :latin-1))
    (clean-lexicon *language*)
    (log:info "Training POS tagger...")
    (maybe-profile (train-tagger *language* pos-train :external-format :latin-1))
    (log:info "Training HMM Chunker...")
    (maybe-profile (train-phrase-extractor *language* chunker-dir))
    (when save-p
      (log:info "Freezing POS database...")
      (maybe-profile (freeze-nlp *language*)))
    (add-language *language*)
    *language*))
