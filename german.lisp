(in-package :nlp)

(defclass german (language)
  ((name :accessor name :initform "german" :initarg :name)
   (aliases :accessor aliases
            :initform '("de" "de-at" "de-de" "de-il" "de-lu" "de-ch")
            :initarg :aliases)
   (vowels :accessor vowels :initform '(#\a #\e #\i #\o #\u #\ä #\ö #\ü)
           :initarg :vowels)
   (default-encoding :accessor default-encoding :initform :utf-8
                     :initarg :default-encoding)))

(defmethod noun-p ((language german) symbol)
  (member symbol '(:NN :NE)))

(defmethod adjective-p ((language german) symbol)
  (member symbol '(:ADJA :ADJD)))

(defmethod verb-p ((language german) symbol)
  (member symbol '(:VAFIN :VAIMP :VAINF :VAPP :VMFIN :VMINF :VMPP :VVFIN :VVIMP :VVINF :VVIZU :VVPP)))

(defmethod adverb-p ((language german) symbol)
  (member symbol '(:ADV :PAV)))

(defmethod determiner-p ((language german) symbol)
  (member symbol '(:ART)))

(defmethod pronoun-p ((language german) symbol)
  (member symbol '(:PDS :PDAT :PIS :PIAT :PIDAT :PPER :PPOSS :PPOSAT :PRELS :PRELAT :PRF :PWS :PWAT :PWAV)))

(defmethod train-phrase-extractor ((language german) file)
  "Train the noun phrase extractor on a given german labeled corpus."
  (reset-chunker-tables language)
  (map-sexp-corpus
   (lambda (tree)
     (learn-phrase-pattern language (flatten-tiger-phrase-tree tree)))
   file)
  (compute-np-observation-likelihoods language)
  (compute-np-ngram-probabilities language)
  language)

(defun make-german-db (&key
                         profile-p
                         save-p
                         (user-lexicon "data/german-lexicon.txt")
                         (stop-words-file "data/german-stop-words.txt")
                         (pos-lex "data/german-pos.txt")
                         (pos-train "data/german-pos.txt")
                         (chunker-train "data/german-parsed.txt"))
  (let ((*language* (make-new-language 'german)))
    (setf (alphabet *language*)
          "áéíóúñü¡abcdefghijklmnopqrstuvwxyz0123456789-'/")
    (log:info "Training German NLP system...")
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
