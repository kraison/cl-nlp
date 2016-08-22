(in-package :nlp)

(defun end-of-sentence-p (word pos)
  ;; FIXME: this could get unwieldy if the pos changes for each language
  (and (or (eql pos :|.|) ;; english, portuguese
           (eql pos :$.) ;; german
           (eql pos :FP)  ;; spanish
           (eql pos :FS) ;; italian
           (eql pos :PONCT)) ;; french
       (scan *punctuation* word)))

(defun read-tree-from-stream (stream)
  (handler-case
      (read stream nil :eof)
    (error (c)
      (log:error "PROBLEM READING TREE: ~A" c)
      nil)))

(defun map-sexp-corpus (fn file &key collect-p
                                  (external-format '(:utf-8 :replacement #\?)))
  "Apply fn to each tree in parsed corpus FILE.
This function assumes that all sensitive characters have been escaped:
s/[.,`':;#|]{1}/\\$1/g"
  (let ((result nil))
    (with-open-file (in file
                        :element-type 'character
                        :external-format external-format)
      (do ((tree (read-tree-from-stream in) (read-tree-from-stream in)))
          ((eql tree :eof))
        (when tree
          (if collect-p
              (push (funcall fn (first tree)) result)
              (funcall fn (first tree))))))
    (nreverse result)))

(defun parse-conll-line (line)
  "Tokenize a line in CoNLL format."
  ;;(let ((columns (split "\\t" line)))
  (let ((columns (split "\\s+" line)))
    (mapcar (lambda (word)
              (list word (fifth columns)))
            (split "\=" (second columns)))))
;    (values (second columns) ;; word
;            (third columns) ;; lemma
;            (fourth columns) ;; general pos
;            (fifth columns)))) ;; specific pos

(defun parse-conll-punct-line (line)
  (register-groups-bind (punctuation)
      ("^\\$([^\\s])+\\s+" line)
    punctuation))

(defun map-conll-corpus (fn file &key collect-p
                                   (external-format '(:utf-8 :replacement #\?)))
  "Apply fn to each entry in a CoNLL formatted file."
  (let ((result nil) (sentence nil) (pos-seq nil) (in-sentence-p nil))
    (with-open-file (stream file
                            :element-type 'character
                            :external-format external-format)
      (do ((line (read-line stream nil :eof) (read-line stream nil :eof)))
          ((eql line :eof))
        (cond ((scan "^(\\#|\\<text|\\<\\/text|\\<s|\\<\\/s)" line)
               nil)
              ((scan "^(1|[0-9]+_1)\\s+" line)
               ;; Beginning of a sentence
               (setq in-sentence-p t)
               (dolist (word-and-pos (parse-conll-line line))
                 (push (first word-and-pos) sentence)
                 (push (second word-and-pos) pos-seq)))
              ((and in-sentence-p (zerop (length line)))
               ;; End of a sentence
               (if collect-p
                   (push (funcall fn (nreverse sentence) (nreverse pos-seq)) result)
                   (funcall fn (nreverse sentence) (nreverse pos-seq)))
               (setq in-sentence-p nil
                     sentence nil
                     pos-seq nil))
              (in-sentence-p
               (if (scan "^\\$" line)
                   (handler-case
                       (let ((token (parse-conll-punct-line line)))
                         (push token sentence)
                         (if (scan *punctuation* token)
                             (push :|.| pos-seq)
                             (push (intern token :keyword) pos-seq)))
                     (error (c)
                       (log:error "Problem parsing '~A'" line)
                       (error c)))
                   (dolist (word-and-pos (parse-conll-line line))
                     (push (first word-and-pos) sentence)
                     (push (second word-and-pos) pos-seq)))))))
    (nreverse result)))

(defun map-xml-corpus (fn directory &key collect-p ignore-file-extension-p)
  "Apply fn to each file in xml corpus directory."
  (let ((result nil))
    (map nil
         (lambda (file)
           (when (or ignore-file-extension-p
                     (cl-ppcre:scan "\.xml$" (namestring file)))
             (let ((tree (s-xml:parse-xml-file file :output-type :sxml)))
               (if collect-p
                   (push (funcall fn tree) result)
                   (funcall fn tree)))))
         (cl-fad:list-directory directory))
    (nreverse result)))

(defun read-tagged-word (stream)
  "Read a tagged word from the stream"
  (let ((word (make-array 0
                          :element-type 'character
                          :fill-pointer t
                          :adjustable t)))
    (peek-char t stream nil :eof)
    (do ((c (read-char stream nil :eof) (read-char stream nil :eof)))
        ((or (eql c :eof) (member c *whitespace*)))
      (vector-push-extend c word))
    (when (> (length word) 0)
      word)))

(defun tagged-split (word)
  "Split a tagged word into word and POS tag"
  (let ((position (position #\/ word :from-end t)))
    (if (numberp position)
        (let ((pos (remove #\^ (subseq word (1+ position)))))
          (values (subseq word 0 position)
                  (intern pos :keyword)))
        (values word nil))))

(defun map-tagged-corpus (fn file &key collect-p
                                    (external-format '(:utf-8 :replacement #\?)))
  "Apply fn to each tagged word in FILE"
  (let ((result nil))
    (with-open-file (in file
                        :element-type 'character
                        :external-format external-format)
      (do ((word (read-tagged-word in) (read-tagged-word in)))
          ((null word))
        (multiple-value-bind (word pos) (tagged-split word)
          (when (and word pos)
            (if collect-p
                (push (funcall fn word pos) result)
                (funcall fn word pos))))))
    (nreverse result)))

(defun extract-tagged-sentences (file &key (external-format :utf-8))
  "Break a tagged file into individual sentences"
  (let ((sentences nil)
	(sentence nil)
	(pos-seqs nil)
	(pos-seq nil))
    (map-tagged-corpus
     (lambda (word pos)
       ;; FIXME: this could get unwieldy if the pos changes for each language
       (if (end-of-sentence-p word pos)
           (unless (null sentence)
             (push pos pos-seq)
             (push word sentence)
             (push (nreverse pos-seq) pos-seqs)
             (push (nreverse sentence) sentences)
             (setq sentence nil pos-seq nil))
           (progn
             (push pos pos-seq)
             (push word sentence))))
     file
     :external-format external-format)
    (values (nreverse sentences) (nreverse pos-seqs))))

(defun split-pos-tags (pos-seq)
  (let ((seqs nil))
    (labels ((walk-it (unprocessed-seq processed-seq)
               (if (null unprocessed-seq)
                   (push processed-seq seqs)
                   (let ((pos (first unprocessed-seq)))
                     (if (find #\| (symbol-name pos))
                         (dolist (pos1 (split "\\|" (symbol-name pos)))
                           (walk-it (rest unprocessed-seq)
                                    (append processed-seq
                                            (list (intern pos1 :keyword)))))
                         (walk-it (rest unprocessed-seq)
                                  (append processed-seq (list pos))))))))
      (walk-it pos-seq nil))
    seqs))

(defun cleanup-corpus (in-file)
  (with-open-file (out (format nil "~A.new" in-file)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((sentence nil)
          (pos-seq nil))
      (map-tagged-corpus
       (lambda (word pos)
         (if (end-of-sentence-p word pos)
             (unless (null sentence)
               (push pos pos-seq)
               (push word sentence)
               (setq sentence (nreverse sentence))
               (dolist (new-pos-seq (split-pos-tags (nreverse pos-seq)))
                 (dotimes (i (length new-pos-seq))
                   (format out "~A/~A" (elt sentence i) (elt new-pos-seq i))
                   (if (or (eql (elt new-pos-seq i) :|.|)
                           (eql (elt new-pos-seq i) :FP))
                       (format out "~%")
                       (format out " "))))
               (setq sentence nil pos-seq nil))
             (progn
               (push pos pos-seq)
               (push word sentence))))
       in-file))))
