(in-package :nlp)

(defclass french (language)
  ((name :accessor name :initform "french" :initarg :name)
   (aliases :accessor aliases
            :initform '("fr" "fr-be" "fr-ca" "fr-fr" "fr-lu" "fr-ch")
            :initarg :aliases)
   (vowels :accessor vowels :initform
           '(#\a #\e #\i #\o #\u #\à #\è #\é #\ì #\í #\î #\ò #\ó #\ù #\ú)
           :initarg :vowels)
   (default-encoding :accessor default-encoding :initform :utf-8
                     :initarg :default-encoding)))

(defmethod noun-p ((language french) symbol)
  (member symbol '(:NC :NPP)))

(defmethod adjective-p ((language french) symbol)
  (member symbol '(:ADJWH :ADJ)))

(defmethod verb-p ((language french) symbol)
  (member symbol '(:V :VIMP :VINF :VPP :VPR :VS)))

(defmethod adverb-p ((language french) symbol)
  (member symbol '(:ADV :ADVWH)))

(defmethod determiner-p ((language french) symbol)
  (member symbol '(:DET :DETWH)))

(defmethod pronoun-p ((language french) symbol)
  (member symbol '(:PRO :PROREL :PROWH)))

(defun convert-sequoia-conll-file (in-file out-file)
  "Convert a Sequoia corpus file into standard word/pos format"
  (with-open-file (stream out-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (map-conll-corpus
     (lambda (sentence pos-seq)
       (dotimes (i (length sentence))
         (format stream "~A/~A" (elt sentence i) (elt pos-seq i))
         (if (= i (- (length sentence) 1))
             (terpri stream)
             (format stream " "))))
     in-file)))

(defun convert-frwikinews-file (in-file out-file)
  "Convert an frwikinews corpus file into standard word/pos format"
  (with-open-file (out-stream out-file
                              :direction :output
                              :element-type 'character
                              :external-format :utf-8
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (with-open-file (in-stream in-file
                               :direction :input
                               :element-type 'character
                               :external-format :utf-8)
      (do ((line (read-line in-stream nil :eof) (read-line in-stream nil :eof)))
          ((eql line :eof))
        (let ((tokens (split "\\s+" line)))
          (dotimes (i (length tokens))
            (let ((token (elt tokens i)))
              (let ((marker-position (position #\_ token :from-end t)))
                (let ((word (subseq token 0 marker-position))
                      (pos (subseq token (1+ marker-position))))
                  (format out-stream "~A/~A" word pos)
                  (if (= i (- (length tokens) 1))
                      (format out-stream "~%")
                      (format out-stream " ")))))))))))

(defun flatten-french-phrase-tree (tree)
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

(defmethod train-phrase-extractor ((language french) file)
  "Train the noun phrase extractor on a given french labeled corpus."
  (reset-chunker-tables language)
  (map-sexp-corpus
   (lambda (tree)
     (learn-phrase-pattern language (flatten-french-phrase-tree tree)))
   file)
  (compute-np-observation-likelihoods language)
  (compute-np-ngram-probabilities language)
  language)

(defun make-french-db (&key
                         profile-p
                         save-p
                         (user-lexicon "data/french-lexicon.txt")
                         (stop-words-file "data/french-stop-words.txt")
                         (pos-lex "data/french-pos.txt")
                         (pos-train "data/french-pos.txt")
                         (chunker-train "data/french-parsed.txt"))
  (let ((old-language (lookup-language "french")))
    (when old-language
      (close-language old-language)))
  (let ((*language* (make-new-language 'french)))
    (setf (alphabet *language*)
          "áéíóúñü¡abcdefghijklmnopqrstuvwxyz0123456789-'/")
    (log:info "Training French NLP system...")
    (log:info "Building and training lexicon...")
    (load-stop-words *language* stop-words-file)
    (maybe-profile
     (make-lexicon *language*
                   pos-lex
                   :user-file user-lexicon
                   :external-format :utf-8))
    (log:info "Training POS tagger...")
    (maybe-profile (train-tagger *language* pos-train :external-format :utf-8))
    (log:info "Training HMM Chunker...")
    (maybe-profile (train-phrase-extractor *language* chunker-train))
    (when save-p
      (log:info "Freezing POS database...")
      (maybe-profile (freeze-nlp *language*)))
    (add-language *language*)
    *language*))
