(in-package :nlp)

(defun make-nlp-dbm (file)
  (when (probe-file file)
    (delete-file file))
  (let ((dbm (make-instance 'kc-dbm)))
    (dbm-open dbm file :create :write)
    dbm))

(defun open-nlp-dbm (file &key (mode :read))
  (let ((dbm (make-instance 'kc-dbm)))
    (dbm-open dbm file mode)
    dbm))

(defun close-nlp-dbm (dbm)
  (when (typep dbm 'kc-dbm)
    (dbm-close dbm)))

;;; POS Tagging observations (word -> (parts of speech & probabilities))
(defmethod make-observations-dbm ((language language) &optional file)
  (unless file
    (setq file (format nil "~A-observations.kch" (name language))))
  (let ((dbm (make-nlp-dbm file)))
    (setf (observations-file language) file
          (observations-dbm language) dbm)
    (clrhash (observations language))
    dbm))

(defmethod open-observations-dbm ((language language) &key file data-dir (mode :read))
  (unless file
    (if data-dir
        (setq file (format nil "~A/~A-observations.kch" data-dir (name language)))
        (setq file (format nil "~A-observations.kch" (name language)))))
  (let ((dbm (open-nlp-dbm file :mode mode)))
    (setf (observations-file language) file
          (observations-dbm language) dbm)
    (clrhash (observations language))
    dbm))

(defmethod close-observations-dbm ((language language))
  (clrhash (observations language))
  (close-nlp-dbm (observations-dbm language)))

(defun serialize-observations-seq (seq)
  (sb-ext:string-to-octets
   (join (mapcar (lambda (pair)
                   (format nil "~A ~S" (car pair) (cdr pair)))
                 seq)
         (format nil "~A" #\Tab))))

(defun deserialize-observations-seq (vector)
  (mapcar (lambda (s)
            (let ((pair (split " " s)))
              (cons (intern (first pair) :keyword)
                    (parse-number:parse-number (second pair)))))
          (split "\\t" (sb-ext:octets-to-string vector))))

(defmethod add-observation-dbm ((language language) word pos probability)
  (setq word (string-downcase word))
  (with-transaction ((observations-dbm language))
    (let* ((pos-vector (dbm-get (observations-dbm language)
                                (sb-ext:string-to-octets word)
                                :octets))
           (pos-seq (when (vectorp pos-vector)
                      (deserialize-observations-seq pos-vector))))
      (setq pos-seq (delete pos pos-seq :key 'car))
      (push (cons pos probability) pos-seq)
      (dbm-remove (observations-dbm language)
                  (sb-ext:string-to-octets word))
      (dbm-put (observations-dbm language)
               (sb-ext:string-to-octets word)
               (serialize-observations-seq pos-seq))
      (remhash word (observations language))
      pos-seq)))

(defmethod replace-observations-dbm ((language language) word pos-seq)
  (setq word (string-downcase word))
  (with-transaction ((observations-dbm language))
    (let ((pos-vector (serialize-observations-seq pos-seq)))
      (dbm-remove (observations-dbm language)
                  (sb-ext:string-to-octets word))
      (dbm-put (observations-dbm language)
               (sb-ext:string-to-octets word)
               pos-vector)
      (remhash word (observations language))
      pos-seq)))

(defmethod lookup-observations-dbm ((language language) word)
  (setq word (string-downcase word))
  (or (gethash word (observations language))
      (let ((pos-string (dbm-get (observations-dbm language)
                                 (sb-ext:string-to-octets word)
                                 :octets)))
        (when pos-string
          (setf (gethash word (observations language))
                (deserialize-observations-seq pos-string))))))

(defmethod remove-from-observations-dbm ((language language) word)
  (setq word (string-downcase word))
  (with-transaction ((observations-dbm language))
    (dbm-remove (observations-dbm language)
                (sb-ext:string-to-octets word)))
  (remhash word (observations language)))

;;; Word occurrence (word -> occurrences)
(defmethod make-word-occurrence-dbm ((language language) &optional file)
  (unless file
    (setq file (format nil "~A-wc.kch" (name language))))
  (let ((dbm (make-nlp-dbm file)))
    (setf (word-occurrence-file language) file
          (word-occurrence-dbm language) dbm)
    (clrhash (word-occurrences language))
    dbm))

(defmethod open-word-occurrence-dbm ((language language) &key file data-dir (mode :read))
  (unless file
    (if data-dir
        (setq file (format nil "~A/~A-wc.kch" data-dir (name language)))
        (setq file (format nil "~A-wc.kch" (name language)))))
  (let ((dbm (open-nlp-dbm file :mode mode)))
    (setf (word-occurrence-file language) file
          (word-occurrence-dbm language) dbm)
    (clrhash (word-occurrences language))
    dbm))

(defmethod close-word-occurrence-dbm ((language language))
  (clrhash (word-occurrences language))
  (close-nlp-dbm (word-occurrence-dbm language)))

(defun serialize-int (int)
  "Encodes integers between (- (1- (expt 2 (* 8 255)))) and
 (1- (expt 2 (* 8 255)))"
  (let* ((n-bytes (ceiling (integer-length int) 8))
         (vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (dotimes (i n-bytes)
      (setf (aref vec i) (ldb (byte 8 (* i 8)) int)))
    vec))

(defun deserialize-int (bytes)
  "Decode a positive integer."
  (declare (type (array (unsigned-byte 8)) bytes))
  (let ((int 0) (n-bytes (length bytes)))
    (dotimes (i n-bytes)
      (setq int (dpb (elt bytes i) (byte 8 (* i 8)) int)))
    int))

(defmethod incf-word-occurrence ((language language) word &optional (delta 1))
  (with-transaction ((word-occurrence-dbm language))
    (setq word (string-downcase word))
    (let ((occurrences (dbm-get (word-occurrence-dbm language)
                                (sb-ext:string-to-octets word)
                                :octets)))
      (when occurrences
        (setq occurrences (deserialize-int occurrences))
        (dbm-remove (word-occurrence-dbm language) word))
      (remhash word (word-occurrences language))
      (dbm-put (word-occurrence-dbm language)
               (sb-ext:string-to-octets word)
               (serialize-int (+ delta (or occurrences 0)))))))

(defmethod lookup-word-occurrence ((language language) word)
  (setq word (string-downcase word))
  (or (gethash word (word-occurrences language))
      (setf (gethash word (word-occurrences language))
            (let ((occurrences
                   (dbm-get (word-occurrence-dbm language)
                            (sb-ext:string-to-octets word)
                            :octets)))
              (if occurrences
                  (deserialize-int occurrences)
                  0)))))

(defmethod remove-from-word-occurrence-dbm ((language language) word)
  (setq word (string-downcase word))
  (remhash word (word-occurrences language))
  (with-transaction ((word-occurrence-dbm language))
    (dbm-remove (word-occurrence-dbm language) (sb-ext:string-to-octets word))))

;;; Regular lexicon (word -> parts of speech)
(defmethod make-lexicon-dbm ((language language) &optional file)
  (unless file
    (setq file (format nil "~A-lex.kch" (name language))))
  (let ((dbm (make-nlp-dbm file)))
    (setf (lexicon-file language) file
          (lexicon-dbm language) dbm)
    dbm))

(defmethod open-lexicon-dbm ((language language) &key file data-dir (mode :read))
  (unless file
    (if data-dir
        (setq file (format nil "~A/~A-lex.kch" data-dir (name language)))
        (setq file (format nil "~A-lex.kch" (name language)))))
  (let ((dbm (open-nlp-dbm file :mode mode)))
    (setf (lexicon-file language) file
          (lexicon-dbm language) dbm)
    dbm))

(defmethod close-lexicon-dbm ((language language))
  (close-nlp-dbm (lexicon-dbm language)))

(defun serialize-pos-seq (seq)
  (sb-ext:string-to-octets (join seq (format nil "~A" #\Tab))))

(defun deserialize-pos-seq (vector)
  (mapcar (lambda (s)
            (intern s :keyword))
          (split "\\t" (sb-ext:octets-to-string vector))))

(defmethod add-to-lexicon-dbm ((language language) word pos)
  (setq word (string-downcase word))
  (with-transaction ((lexicon-dbm language))
    (let* ((pos-vector (dbm-get (lexicon-dbm language)
                                (sb-ext:string-to-octets word)
                                :octets))
           (pos-seq (when (vectorp pos-vector)
                      (deserialize-pos-seq pos-vector))))
      (unless (find pos pos-seq)
        (push pos pos-seq)
        (dbm-remove (lexicon-dbm language) word)
        (dbm-put (lexicon-dbm language)
                 (sb-ext:string-to-octets word)
                 (serialize-pos-seq pos-seq)))
      pos-seq)))

(defmethod lookup-pos-dbm ((language language) word)
  (setq word (string-downcase word))
  (let ((pos-vector (dbm-get (lexicon-dbm language)
                             (sb-ext:string-to-octets word)
                             :octets)))
    (when pos-vector
      (deserialize-pos-seq pos-vector))))

(defmethod remove-from-lexicon-dbm ((language language) word)
  (setq word (string-downcase word))
  (with-transaction ((lexicon-dbm language))
    (dbm-remove (lexicon-dbm language) (sb-ext:string-to-octets word))))

;;; Probabilistic lexicon (word -> part of speech & probability)
(defmethod make-plexicon-dbm ((language language) &optional file)
  (unless file
    (setq file (format nil "~A-plex.kch" (name language))))
  (let ((dbm (make-nlp-dbm file)))
    (setf (plexicon-file language) file
          (plexicon-dbm language) dbm)
    dbm))

(defmethod open-plexicon-dbm ((language language) &key file data-dir (mode :read))
  (unless file
    (if data-dir
        (setq file (format nil "~A/~A-plex.kch" data-dir (name language)))
        (setq file (format nil "~A-plex.kch" (name language)))))
  (let ((dbm (open-nlp-dbm file :mode mode)))
    (setf (plexicon-file language) file
          (plexicon-dbm language) dbm)
    dbm))

(defmethod close-plexicon-dbm ((language language))
  (close-nlp-dbm (plexicon-dbm language)))

(defun serialize-ppos-seq (seq)
  (sb-ext:string-to-octets
   (join (mapcar (lambda (pair)
                   (format nil "~A ~A" (car pair) (cdr pair)))
                 seq)
         (format nil "~A" #\Tab))))

(defun deserialize-ppos-seq (vector)
  (mapcar (lambda (s)
            (let ((pair (split " " s)))
              (cons (intern (first pair) :keyword)
                    (parse-number:parse-number (second pair)))))
          (split "\\t" (sb-ext:octets-to-string vector))))

(defmethod add-to-plexicon-dbm ((language language) word pos probability)
  (setq word (string-downcase word))
  (with-transaction ((plexicon-dbm language))
    (let* ((pos-vector (dbm-get (plexicon-dbm language)
                                (sb-ext:string-to-octets word)
                                :octets))
           (pos-seq (when (vectorp pos-vector)
                      (deserialize-ppos-seq pos-vector))))
      (setq pos-seq (delete pos pos-seq :key 'car))
      (push (cons pos probability) pos-seq)
      (dbm-remove (plexicon-dbm language)
                  (sb-ext:string-to-octets word))
      (dbm-put (plexicon-dbm language)
               (sb-ext:string-to-octets word)
               (serialize-ppos-seq pos-seq))
      pos-seq)))

(defmethod lookup-ppos-dbm ((language language) word)
  (setq word (string-downcase word))
  (let ((pos-string (dbm-get (plexicon-dbm language)
                             (sb-ext:string-to-octets word)
                             :octets)))
    (when pos-string
      (deserialize-ppos-seq pos-string))))

(defmethod remove-from-plexicon-dbm ((language language) word)
  (setq word (string-downcase word))
  (with-transaction ((plexicon-dbm language))
    (dbm-remove (plexicon-dbm language)
                (sb-ext:string-to-octets word))))

#|
(defun test-kyoto-lexicon ()
  (let ((language (make-instance 'language :name "test-language")))
    (make-lexicon-dbm language)
    (make-plexicon-dbm language)
    (unwind-protect
         (progn
           (format t "~A~%" (add-to-lexicon-dbm language "test" :NN))
           (format t "~A~%" (add-to-lexicon-dbm language "test" :NNP))
           (format t "~A~%" (add-to-lexicon-dbm language "test" :VB))

           (format t "~A~%" (add-to-plexicon-dbm language "test" :NN 0.1))
           (format t "~A~%" (add-to-plexicon-dbm language "test" :NNP 0.1))
           (format t "~A~%" (add-to-plexicon-dbm language "test" :VB 0.8))

           (format t "~A~%" (lookup-pos-dbm language "test"))
           (format t "~A~%" (lookup-ppos-dbm language "test"))

           (format t "são: ~A~%" (add-to-lexicon-dbm language "são" :NN))
           (format t "são: ~A~%" (add-to-lexicon-dbm language "são" :NNP))
           (format t "são: ~A~%" (add-to-lexicon-dbm language "são" :VB))

           (format t "são: ~A~%" (add-to-plexicon-dbm language "são" :NN 0.1))
           (format t "são: ~A~%" (add-to-plexicon-dbm language "são" :NNP 0.1))
           (format t "são: ~A~%" (add-to-plexicon-dbm language "são" :VB 0.8))

           (format t "são: ~A~%" (lookup-pos-dbm language "são"))
           (format t "são: ~A~%" (lookup-ppos-dbm language "são"))

           (dump-lexicon language "test-language.lex"))
      (progn
        (close-lexicon-dbm language)
        (delete-file (lexicon-file language))

        (close-plexicon-dbm language)
        (delete-file (plexicon-file language))))
    language))
|#
