(in-package :nlp)

(defvar *number-regex* (create-scanner "^[0-9\-./,:]+\\%?$" :single-line-mode t))
(defvar *host-regex*
  (create-scanner
   "([a-z\\d]([a-z\\d\\-]{0,61}[a-z\\d])?(\\.[a-z\\d]([a-z\\d\\-]{0,61}[a-z\\d])?)+)"
   :single-line-mode t :case-insensitive-mode t))
(defvar *ip-regex*
  (create-scanner
   "(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])"
   :single-line-mode t :case-insensitive-mode t))

(let ((hex-regex
       (create-scanner "^0x[ABCDEF0123456789]+$"
                       :single-line-mode t
                       :case-insensitive-mode t)))
  (defun hex-number-p (text)
    (scan hex-regex text)))

(defun translate-moby-to-penn (pos word)
  "Translate a Moby POS tag to a Penn tag"
  (case pos
    (#\N (if (upper-case-p (elt word 0)) :NNP :NN))
    (#\p (if (upper-case-p (elt word 0)) :NNPS :NNS))
    (#\h :NN)
    (#\V :VBG)
    (#\t :VB)
    (#\i :VB)
    (#\A :JJ)
    (#\v :RB)
    (#\C :CC)
    (#\P :IN)
    (#\! :UH)
    (#\r :PRP)
    (#\D :DT)
    (#\I :DT)
    (#\o  nil)
    (:otherwise nil)))

(defun translate-moby-to-generic (pos)
  "Translate a Moby POS tag to a generic tag"
  (case pos
    (#\N +noun-tag+)
    (#\p +noun-tag+)
    (#\h +noun-tag+)
    (#\V +verb-tag+)
    (#\t +verb-tag+)
    (#\i +verb-tag+)
    (#\A +adjective-tag+)
    (#\v +adverb-tag+)
    (#\C +conjunction-tag+)
    (#\P +adposition-tag+)
    (#\! +foreign-tag+)
    (#\r +pronoun-tag+)
    (#\D +determiner-tag+)
    (#\I +determiner-tag+)
    (#\o  nil)
    (:otherwise nil)))

(defmethod map-lexicon (fn (language language) &key collect-p)
  "Apply a function to each entry in a language's lexicon;  fn should take 2
args: the word and a list of its parts of speech."
  (let ((result nil)
        (iterator (iter-open (lexicon-dbm language))))
    (unwind-protect
         (progn
           (iter-first iterator)
           (loop
              (multiple-value-bind (word-vector pos-vector)
                  (iter-item iterator :value-type :octets :key-type :octets)
                (when (zerop (length word-vector))
                  (return))
                (let ((pos-seq (deserialize-pos-seq pos-vector))
                      (word (sb-ext:octets-to-string word-vector)))
                  (if collect-p
                      (push (funcall fn word pos-seq) result)
                      (funcall fn word pos-seq))))
              (iter-next iterator)))
      (iter-close iterator))
    (nreverse result)))

(defmethod dump-lexicon ((language language) file &key include-pos-p (external-format :utf-8))
  "Write lexicon (and optionally parts-of-seech) to a text file."
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'character
                          :external-format external-format)
    (map-lexicon (lambda (word pos-seq)
                   (format stream "~A" word)
                   (when include-pos-p
                     (format stream "~A~S" #\Tab pos-seq))
                   (terpri stream))
                 language)))

(defmethod augment-lexicon ((language language) file counts frequencies &key type (external-format :utf-8))
  "Augment our learned lexicon with Moby's POS lexicon or a user-supplied lexicon using Penn tags."
  (with-open-file (stream file
                          :element-type 'character
                          :external-format external-format)
    (do ((line (read-line stream nil :eof) (read-line stream nil :eof)))
        ((eq line :eof))
      (when (> (length line) 1)
        (destructuring-bind (word pos) (split "\\\\" line :limit 2)
          (let ((pos-list
                 (case type
                   (:moby
                    (remove-if (lambda (p) (or (null p) (eq :NP p)))
                               (remove-duplicates
                                (map 'list (lambda (p)
                                             ;;(translate-moby-to-generic p))
                                             (translate-moby-to-penn p word))
                                     pos))))
                   (otherwise
                    (mapcar (lambda (p)
                              ;;(lookup-generic-pos :english p))
                              (intern (string-upcase p) :keyword))
                            (split "\\s+" pos))))))
            (incf (gethash word frequencies 0))
            (dolist (p pos-list)
              (add-to-lexicon-dbm language word p)
              (unless (hash-table-p (gethash word counts))
                (setf (gethash word counts) (make-hash-table)))
              (setf (gethash p (gethash word counts))
                    (1+ (gethash p (gethash word counts) 1))))))))))

(defmethod make-lexicon ((language language) in-file
                         &key (equality 'equalp) out-file moby-file user-file
                           (external-format :utf-8))
  "Derive a lexicon from a tagged corpus.  Also accepts a Moby parts-of-speech file
as an optional argument;  will translate the Moby data into Penn tags and add to
the lexicon.  Also takes an additional user-file with either Moby or Penn style
tags."
  (let ((counts (make-hash-table :test equality))
        (frequencies (make-hash-table :test 'equal)))
    (map-tagged-corpus
     (lambda (word pos)
       (setf (gethash word frequencies)
             (1+ (gethash word frequencies 1)))
       (add-to-lexicon-dbm language word pos)
       (unless (hash-table-p (gethash word counts))
         (setf (gethash word counts) (make-hash-table)))
       (setf (gethash pos (gethash word counts))
             (1+ (gethash pos (gethash word counts) 1))))
     in-file
     :external-format external-format)
    (when (and moby-file (probe-file moby-file))
      (augment-lexicon language moby-file counts frequencies
                       :type :moby
                       :external-format external-format))
    (when (and user-file (probe-file user-file))
      (augment-lexicon language user-file counts frequencies
                       :type :other
                       :external-format external-format))
    (maphash
     (lambda (word table)
       (let ((total (loop
                       for pos being the hash-keys of table
                       using (hash-value c)
                       summing c into total
                       finally (return total))))
         (maphash (lambda (pos count)
                    (add-to-plexicon-dbm language word pos (/ count total)))
                  table)))
     counts)
    (when out-file
      (dump-lexicon language out-file :include-pos-p t))
    language))

(defgeneric in-lexicon-p (language word pos)
  (:method ((language language) word pos)
    (when (symbolp word) (setq word (symbol-name word)))
    (or (member pos (lookup-pos-dbm language word))
        (member pos (lookup-pos-dbm language (string-downcase word))))))

(defgeneric lookup-pos (language word)
  (:method ((language language) word)
    (or (lookup-pos-dbm language word)
        (lookup-pos-dbm language (string-downcase word)))))

(defun is-a (word pos &optional (language *language*))
  (member pos (lookup-pos language word)))

(defun add-to-lexicon (word pos &optional (language *language*))
  "Add a word to the lexicon"
  (add-to-lexicon-dbm language word pos))

(defmethod add-pos-regex ((language language) regex pos)
  (push (cons (create-scanner regex :single-line-mode t :case-insensitive-mode t)
              pos)
        (user-pos-regex language)))

(defmethod lookup-ppos ((language language) word pos)
  (or (cdr (assoc pos (lookup-ppos-dbm language word))) 0))

(defmethod pos-probabilities ((language language) word)
  "Parts of speech with learned probabilities for WORD"
  (lookup-ppos-dbm language word))

;; Spelling corrector based on Norvig's:
;; http://mikael.jansson.be/log/spellcheck-in-lisp
(defmethod edits-1 ((language language) word)
  "Find edits of one character"
  (let* ((splits (loop for i from 0 upto (length word)
                    collecting (cons (subseq word 0 i) (subseq word i))))
         (deletes (loop for (a . b) in splits
                     when (not (zerop (length b)))
                     collect (concatenate 'string a (subseq b 1))))
         (transposes (loop for (a . b) in splits
                        when (> (length b) 1)
                        collect (concatenate 'string a (subseq b 1 2)
                                             (subseq b 0 1) (subseq b 2))))
         (replaces (loop for (a . b) in splits
                      nconcing (loop for c across (alphabet language)
                                  when (not (zerop (length b)))
                                  collect (concatenate 'string a (string c)
                                                       (subseq b 1)))))
         (inserts (loop for (a . b) in splits
                     nconcing (loop for c across (alphabet language)
                                 collect (concatenate 'string a (string c) b)))))
    (delete-duplicates
     (nconc deletes transposes replaces inserts)
     :test 'equal)))

(defmethod known-edits-2 ((language language) word)
  "Find edits of 2 characters"
  (remove-duplicates
   (loop for e1 in (edits-1 language word) nconcing
        (loop for e2 in (edits-1 language e1)
           when (let ((freq (lookup-word-occurrence language e2)))
                  (> freq 0))
           collect e2))
   :test 'equal))

(defmethod known ((language language) words)
  "Remove unknown words from list"
  (loop for word in words
     when (let ((freq (lookup-word-occurrence language word)))
            (> freq 0))
     collect word))

(defmethod correct-spelling ((language language) word)
  "Correct spelling for a single word"
  (let ((winner word) (max-f 0))
    (dolist (n-word (or (known language (list word))
                        (known language (edits-1 language word))
                        (known-edits-2 language word)
                        (list word)))
      (let ((f (lookup-word-occurrence language n-word)))
        (when (> f max-f)
          (setq winner n-word max-f f))))
    winner))

(defmethod spell-check ((language language) text &key join?)
  "Correct spelling in arbitrary text"
  (let ((words (mapcar (lambda (word)
                         (loop for word in (or (known language (list word))
                                               (known language (edits-1 language word))
                                               (known-edits-2 language word)
                                               (list word))
                            maximizing (lookup-word-occurrence language word)
                            finally (return word)))
                       (tokenize language text))))
    (if join?
        (if (member (last1 words) '(#\. #\? #\!))
            (format nil "~{~A~^ ~}~A" (butlast words) (last1 words))
            (format nil "~{~A~^ ~}" words))
        words)))
