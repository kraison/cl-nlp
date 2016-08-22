(in-package :nlp)

(defclass italian (language)
  ((name :accessor name :initform "italian" :initarg :name)
   (aliases :accessor aliases
            :initform '("it" "it-it" "it-ch")
            :initarg :aliases)
   (vowels :accessor vowels :initform
           '(#\a #\e #\i #\o #\u #\à #\è #\é #\ì #\í #\î #\ò #\ó #\ù #\ú)
           :initarg :vowels)
   (default-encoding :accessor default-encoding :initform :utf-8
                     :initarg :default-encoding)))

(defmethod noun-p ((language italian) symbol)
  (member symbol '(:N :S :SP)))

(defmethod adjective-p ((language italian) symbol)
  (member symbol '(:A :AP)))

(defmethod verb-p ((language italian) symbol)
  (member symbol '(:V)))

(defmethod adverb-p ((language italian) symbol)
  (member symbol '(:B :BN)))

(defmethod determiner-p ((language italian) symbol)
  (member symbol '(:D :T :DD :DE :DQ :DR)))

(defmethod pronoun-p ((language italian) symbol)
  (member symbol '(:P :PC :PD :PE :PE :PP :PQ :PR)))

(defmethod load-contraction-table ((language italian)
                                   &optional
                                     (file "data/italian-contractions.txt"))
  (when (probe-file file)
    (with-open-file (in file)
      (setf (contraction-table language) (make-hash-table :test 'equalp))
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (multiple-value-bind (match-p matches)
            (scan-to-strings "^([a-zA-Z']+)\\s+(.*)$" line)
          (when match-p
            (setf (gethash (elt matches 0) (contraction-table language))
                  (split "\\s+" (elt matches 1)))))))))

(defun make-italian-db (&key
                          profile-p
                          save-p
                          (contraction-file "data/italian-contractions.txt")
                          (user-lexicon "data/italian-lexicon.txt")
                          (stop-words-file "data/italian-stop-words.txt")
                          (pos-lex "data/italian-pos-all.txt")
                          (pos-train "data/italian-pos.txt")
                          (chunker-train "data/italian-parsed.txt"))
  (let ((*language* (make-new-language 'italian)))
    (setf (alphabet *language*)
          "àèéìíîòóùúabcdefghijklmnopqrstuvwxyz0123456789-'/")
    (log:info "Training Italian NLP system...")
    (log:info "Building and training lexicon...")
    (load-stop-words *language* stop-words-file)
    (load-contraction-table *language* contraction-file)
    (maybe-profile
     (make-lexicon *language*
                   pos-lex
                   :user-file user-lexicon
                   :external-format :utf-8))
    ;;(clean-lexicon *language*)
    (log:info "Training POS tagger...")
    (maybe-profile (train-tagger *language* pos-train :external-format :utf-8))
    (log:info "Training HMM Chunker...")
    (maybe-profile (train-phrase-extractor *language* chunker-train))
    (when save-p
      (log:info "Freezing POS database...")
      (maybe-profile (freeze-nlp *language*)))
    (add-language *language*)
    *language*))
