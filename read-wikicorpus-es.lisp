(in-package :nlp)

(def-pos-map :wiki-es
    (("number" "NUM")
     ("frc" ".")
     ("flt" ".")
     ("fla" ".")
     ("fra" ".")
     ("ft" ".")
     ("fa" ".")
     ("fc" ".")
     ("fd" ".")
     ("fe" ".")
     ("fg" ".")
     ("fh" ".")
     ("fi" ".")
     ("fp" ".")
     ("fs" ".")
     ("fx" ".")
     ("fz" ".")
     ("zm" "NUM")
     ("zp" "NUM")
     ("ao" "ADJ")
     ("aq" "ADJ")
     ("cc" "CONJ")
     ("cs" "CONJ")
     ("da" "DET")
     ("dd" "DET")
     ("de" "DET")
     ("di" "DET")
     ("dn" "DET")
     ("dp" "DET")
     ("dt" "DET")
     ("nc" "NOUN")
     ("np" "NOUN")
     ("p0" "PRON")
     ("pd" "PRON")
     ("pe" "PRON")
     ("pi" "PRON")
     ("pn" "PRON")
     ("pp" "PRON")
     ("pr" "PRON")
     ("pt" "PRON")
     ("px" "PRON")
     ("rg" "ADV")
     ("rn" "ADV")
     ("sn" "ADP")
     ("sp" "ADP")
     ("va" "VERB")
     ("vm" "VERB")
     ("vs" "VERB")
     ("w" "NOUN")
     ("z" "NUM")
     ("x" "X")
     ("y" "X")
     ("i" "X")))

(defmethod lookup-generic-pos ((name (eql :wiki-es)) pos &key &allow-other-keys)
  (let ((alist (gethash name *pos-maps*)))
    (when alist
      (cdr (assoc pos alist
                  :test (lambda (key1 key2)
                          (let ((regex (cl-ppcre:create-scanner
                                        (format nil "^~A" key2)
                                        :case-insensitive-mode t)))
                            (scan regex key1))))))))


(defmethod lookup-specific-pos ((name (eql :wiki-es)) pos &key &allow-other-keys)
  (let ((alist (gethash name *pos-maps*)))
    (when alist
      (let ((tag
             (car (assoc pos alist
                         :test (lambda (key1 key2)
                                 (let ((regex (cl-ppcre:create-scanner
                                               (format nil "^~A" key2)
                                               :case-insensitive-mode t)))
                                   (scan regex key1)))))))
        (when tag
          (intern (string-upcase tag) :keyword))))))

(defun read-wiki-es (directory)
  (let ((sentences nil))
    (dolist (file (cl-fad:list-directory directory))
      (let ((sentence nil))
        (with-open-file (in file
                            :element-type 'character
                            :external-format :latin-1)
          (do ((line (read-line in nil :eof) (read-line in nil :eof)))
              ((eql line :eof))
            (cond ((scan "^\<\/doc\>" line)
                   (when sentence
                     (push (nreverse sentence) sentences)
                     (setq sentence nil))
                   ;;(return-from read-wiki-es sentences)
                   )
                  ((scan "^\<doc" line)
                   (setq sentence nil))
                  (t
                   (let ((pieces (split "\\s+" line)))
                     (when pieces
                       (let ((pos (lookup-specific-pos :wiki-es (third pieces))))
                         (unless pos
                           (error "UNKNOWN POS IN ~A: ~S" file (third pieces)))
                         (push (cons pos (first pieces))
                               sentence))))))))))
    sentences))

(defun flatten-wiki-es (&key (directory "data/wiki-es/"))
  (with-open-file (stream "data/wiki-es-pos.txt"
                          :direction :output
                          :element-type 'character
                          :external-format :latin-1
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (file (cl-fad:list-directory directory))
      (let ((sentence nil))
        (log:info "Working on ~A" file)
        (with-open-file (in file
                            :element-type 'character
                            :external-format :latin-1)
          (do ((line (read-line in nil :eof) (read-line in nil :eof)))
              ((eql line :eof))
            (cond ((scan "^\<\/doc\>" line)
                   (when sentence
                     (setq sentence (nreverse sentence))
                     (dotimes (i (length sentence))
                       (format stream "~A/~A"
                               (cdr (nth i sentence))
                               (car (nth i sentence)))
                       (unless (= i (1- (length sentence)))
                         (format stream " ")))
                     (terpri stream)
                     (setq sentence nil))
                   ;;(return-from read-wiki-es sentences)
                   )
                  ((scan "^\<doc" line)
                   (setq sentence nil))
                  (t
                   (let ((pieces (split "\\s+" line)))
                     (when pieces
                       (let ((pos (lookup-specific-pos :wiki-es (third pieces))))
                         (unless pos
                           (error "UNKNOWN POS IN ~A: ~S" file (third pieces)))
                         (push (cons pos (first pieces))
                               sentence))))))))))))
