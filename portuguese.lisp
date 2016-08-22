(in-package :nlp)

(defclass portuguese (language)
  ((name :accessor name :initform "portuguese" :initarg :name)
   (aliases :accessor aliases
            :initform '("pt" "pt-br" "pt-pt")
            :initarg :aliases)
   (vowels :accessor vowels :initform '(#\a #\e #\i #\o #\u #\â #\á #\ã #\ê #\é #\ó #\ô)
           :initarg :vowels)
   (default-encoding :accessor default-encoding :initform :latin-1
                     :initarg :default-encoding)))

(defmethod noun-p ((language portuguese) symbol)
  (member symbol '(:N :PROP)))

(defmethod adjective-p ((language portuguese) symbol)
  (member symbol '(:ADJ)))

(defmethod verb-p ((language portuguese) symbol)
  (member symbol '(:V)))

(defmethod adverb-p ((language portuguese) symbol)
  (member symbol '(:ADV)))

(defmethod determiner-p ((language portuguese) symbol)
  (member symbol '(:DET)))

(defmethod pronoun-p ((language portuguese) symbol)
  (member symbol '(:SPEC :PERS)))

(defun make-portuguese-db (&key
                             profile-p
                             save-p
                             (user-lexicon "data/portuguese-lexicon.txt")
                             (stop-words-file "data/portuguese-stop-words.txt")
                             (pos-lex "data/portuguese-pos.txt")
                             (pos-train "data/portuguese-pos.txt")
                             (chunker-train "data/portuguese-parsed.txt"))
  (let ((*language* (make-new-language 'portuguese)))
    (setf (alphabet *language*)
          "âáãêéóô¡abcdefghijklmnopqrstuvwxyz0123456789-'/")
    (log:info "Training Portuguese NLP system...")
    (log:info "Building and training lexicon...")
    (load-stop-words *language* stop-words-file)
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
