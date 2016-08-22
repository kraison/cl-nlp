(in-package :nlp)

;;; Keeping this file for posterity;  replaced with FFI to libstemmer.

(defmethod initialize-stemmed-word ((language spanish) (string string))
  (let ((word
         (make-instance 'stemmed-word
                        :original-word string
                        :letters
                        (make-array (length string)
                                    :element-type 'character
                                    :fill-pointer (length string)
                                    :initial-contents (string-downcase string)))))
    (find-rv language word)
    (find-r1-r2 language word)
    word))

(defmethod find-rv ((language spanish) (word stemmed-word))
  "If the second letter is a consonant, RV is the region after the next following vowel, or if the first two letters are vowels, RV is the region after the next consonant, and otherwise (consonant-vowel case) RV is the region after the third letter. But RV is the end of the word if these positions cannot be found."
  (let ((rv-idx nil) (rv nil) (word-length (word-length word)))
    (when (> word-length 1)
      (cond ((not (member (elt (letters word) 1) (vowels language)))
             (loop
                for i from 2 below word-length
                until rv
                do
                  (when (member (elt (letters word) i) (vowels language))
                    (setq rv-idx (1+ i)
                          rv (subseq (letters word) rv-idx)))))
            ((and (member (elt (letters word) 0) (vowels language))
                  (member (elt (letters word) 1) (vowels language)))
             (loop
                for i from 2 below word-length
                until rv
                do
                  (when (not (member (elt (letters word) i) (vowels language)))
                    (setq rv-idx (1+ i)
                          rv (subseq (letters word) rv-idx))))))
      (when (and (not rv) (> word-length 3))
        (setq rv-idx 3
              rv (subseq (letters word) rv-idx)))
      (setf (rv word) rv
            (rv-idx word) rv-idx)
      (values rv-idx rv))))

(let* ((suffixes (list "selas" "selos" "sela" "selo" "las" "les"
                       "los" "nos" "me" "se" "la" "le" "lo"))
       (regex1 (create-scanner
                (format nil "(iéndo|ándo|ár|ér|ír)(~{~A~^|~})$" suffixes)))
       (regex2 (create-scanner
                (format nil "(ando|iendo|ar|er|ir)(~{~A~^|~})$" suffixes)))
       (regex3 (create-scanner
                (format nil "u?yendo(~{~A~^|~})$" suffixes)))
       (regex4 (create-scanner
                (format nil "uyendo(~{~A~^|~})$" suffixes))))
  (defmethod do-stem-step-0 ((language spanish) (word stemmed-word))
    (cond ((scan regex1 (rv word))
           ;; FIXME: remove acute accent!
           (setf (fill-pointer (letters word)) (scan regex1 (letters word))))
          ((scan regex2 (rv word))
           (setf (fill-pointer (letters word)) (scan regex2 (letters word))))
          ((scan regex3 (rv word))
           (setf (fill-pointer (letters word)) (scan regex4 (letters word)))))))

(let ((suffixes
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list "amientos" "imientos" "amiento" "imiento" "anzas" "ismos" "ables" "ibles"
                      "istas" "anza" "icos" "icas" "ismo" "able" "ible" "ista" "osos" "osas" "ico"
                      "ica" "oso" "osa")))))
  (defmethod do-stem-step-1a ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (when (scan suffixes (r2 word))
      (let ((position (scan suffixes (letters word))))
        (setf (fill-pointer (letters word)) position)))))

(let ((suffixes-1
       (create-scanner
        (format nil "ic(~{~A~^|~})$"
                (list "aciones" "adoras" "adores" "ancias" "adora" "ación"
                      "antes" "ancia" "ador" "ante"))))
      (suffixes-2
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list "aciones" "adoras" "adores" "ancias" "adora" "ación"
                      "antes" "ancia" "ador" "ante")))))
  (defmethod do-stem-step-1b ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (cond ((scan suffixes-1 (r2 word))
           (let ((position (scan suffixes-1 (letters word))))
             (setf (fill-pointer (letters word)) position)))
          ((scan suffixes-2 (r2 word))
           (let ((position (scan suffixes-2 (letters word))))
             (setf (fill-pointer (letters word)) position))))))

(let ((suffixes
       (create-scanner
        (format nil "(~{~A~^|~})$" (list "logías" "logía")))))
  (defmethod do-stem-step-1c ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (when (scan suffixes (r2 word))
      (let ((position (scan suffixes (letters word))))
        (setf (fill-pointer (letters word)) (+ position 3))))))

(let ((suffixes
       (create-scanner
        (format nil "(~{~A~^|~})$" (list "uciones" "ución")))))
  (defmethod do-stem-step-1d ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (when (scan suffixes (r2 word))
      (let ((position (scan suffixes (letters word))))
        (setf (fill-pointer (letters word)) (+ position 1))))))

(let ((suffixes
       (create-scanner
        (format nil "(~{~A~^|~})$" (list "encias" "encia")))))
  (defmethod do-stem-step-1e ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (when (scan suffixes (r2 word))
      (let ((position (scan suffixes (letters word))))
        (setf (aref (letters word) position) #\e)
        (setf (aref (letters word) (+ 1 position)) #\n)
        (setf (aref (letters word) (+ 2 position)) #\t)
        (setf (aref (letters word) (+ 3 position)) #\e)
        (setf (fill-pointer (letters word)) (+ position 4))))))

(let ((suffix (create-scanner "amente$")))
  (defmethod do-stem-step-1f ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (cond ((scan "ativamente$" (r2 word))
           (let ((position (scan "ativamente$" (letters word))))
             (setf (fill-pointer (letters word)) position)))
          ((scan "(iv|os|ic|ad)amente$" (r2 word))
           (let ((position (scan "(iv|os|ic|ad)amente$" (letters word))))
             (setf (fill-pointer (letters word)) position)))
          ((scan suffix (r1 word))
           (let ((position (scan suffix (letters word))))
             (setf (fill-pointer (letters word)) position))))))

(let ((suffix-1 (create-scanner "(ante|able|ible)mente$"))
      (suffix-2 (create-scanner "mente$")))
  (defmethod do-stem-step-1g ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (cond ((scan suffix-1 (r2 word))
           (setf (fill-pointer (letters word)) (scan suffix-1 (letters word))))
          ((scan suffix-2 (r2 word))
           (setf (fill-pointer (letters word)) (scan suffix-2 (letters word)))))))

(let ((suffixes-1 (create-scanner "(ic|abil|iv)(idades|idad)$"))
      (suffixes-2 (create-scanner "(idades|idad)$")))
  (defmethod do-stem-step-1h ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (cond ((scan suffixes-1 (r2 word))
           (let ((position (scan suffixes-1 (letters word))))
             (setf (fill-pointer (letters word)) position)))
          ((scan suffixes-2 (r2 word))
           (let ((position (scan suffixes-2 (letters word))))
             (setf (fill-pointer (letters word)) position))))))

(let ((suffixes-1 (create-scanner "at(ivos|ivas|ivo|iva)$"))
      (suffixes-2 (create-scanner "(ivos|ivas|ivo|iva)$")))
  (defmethod do-stem-step-1i ((language spanish) (word stemmed-word))
    (find-r1-r2 language word)
    (cond ((scan suffixes-1 (r2 word))
           (let ((position (scan suffixes-1 (letters word))))
             (setf (fill-pointer (letters word)) position)))
          ((scan suffixes-2 (r2 word))
           (let ((position (scan suffixes-2 (letters word))))
             (setf (fill-pointer (letters word)) position))))))

(defmethod do-stem-step-1 ((language spanish) (word stemmed-word))
  "Search for the longest among the following suffixes, and perform the action indicated."
  (let ((modified-p nil))
    (when (do-stem-step-1a language word)
      (setq modified-p t))
    (when (do-stem-step-1b language word)
      (setq modified-p t))
    (when (do-stem-step-1c language word)
      (setq modified-p t))
    (when (do-stem-step-1d language word)
      (setq modified-p t))
    (when (do-stem-step-1e language word)
      (setq modified-p t))
    (when (do-stem-step-1f language word)
      (setq modified-p t))
    (when (do-stem-step-1g language word)
      (setq modified-p t))
    (when (do-stem-step-1h language word)
      (setq modified-p t))
    (when (do-stem-step-1i language word)
      (setq modified-p t))
    modified-p))

(let ((suffixes
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list "yeron" "yendo" "yamos" "yais" "yan" "yen" "yas" "yes" "ya" "ye" "yo" "yó")))))
  (defmethod do-stem-step-2a ((language spanish) (word stemmed-word))
    (when (scan suffixes (rv word))
      (let ((position (scan suffixes (letters word))))
        (when (eql #\u (elt (letters word) (- position 1)))
          (setf (fill-pointer (letters word)) position))))))

(let ((endings-1a
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list
                 "aríamos" "eríamos" "iríamos" "iéramos" "iésemos"))))
      (endings-1b
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list "aríais" "aremos" "eríais" "eremos" "iríais" "iremos" "ierais"
                      "ieseis" "asteis" "isteis" "ábamos" "áramos" "ásemos"))))
      (endings-1c
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list  "arían" "arías" "aréis" "erían" "erías" "eréis" "irían"
                       "irías" "iréis" "ieran" "iesen" "ieron" "iendo" "ieras"
                       "ieses" "abais" "arais" "aseis" "íamos"))))
      (endings-1d
       (create-scanner
        (format nil "(~{~A~^|~})$"
                (list "arán" "arás" "aría" "erán" "erás" "ería" "irán"
                      "irás" "iría" "iera" "iese" "aste" "iste" "aban" "aran" "asen" "aron" "ando"
                      "abas" "adas" "idas" "aras" "ases" "íais" "ados" "idos" "amos" "imos"))))
      (endings-2 (create-scanner "emos$"))
      (endings-3
       (create-scanner (format nil "(~{~A~^|~})$" (list "ará" "aré" "erá" "eré"
                                                        "irá" "iré" "aba" "ada"
                                                        "ida" "ara" "ase" "ían"
                                                        "ado" "ido" "ías" "áis"))))
      (endings-4 (create-scanner "éis$"))
      (endings-5
       (create-scanner (format nil "(~{~A~^|~})$"
                               (list "ía" "ad" "ed" "id" "an" "ió" "ar" "er" "ir" "as" "ís"))))
      (endings-6
       (create-scanner (format nil "(~{~A~^|~})$" (list "es" "en")))))
  (defmethod do-stem-step-2b ((language spanish) (word stemmed-word))
    (find-rv language word)
    (cond
      ((scan endings-1a (rv word))
       (setf (fill-pointer (letters word)) (scan endings-1a (letters word))))
      ((scan endings-1b (rv word))
       (setf (fill-pointer (letters word)) (scan endings-1b (letters word))))
      ((scan endings-1c (rv word))
       (setf (fill-pointer (letters word)) (scan endings-1c (letters word))))
      ((scan endings-1d (rv word))
       (setf (fill-pointer (letters word)) (scan endings-1d (letters word))))
      ((scan endings-2 (rv word))
       (let ((position (scan endings-2 (letters word))))
         (if (search "gu" (letters word) :from-end t :start2 (- position 2) :end2 position)
             (setf (fill-pointer (letters word)) (- position 1))
             (setf (fill-pointer (letters word)) position))))
      ((scan endings-3 (rv word))
       (setf (fill-pointer (letters word)) (scan endings-3 (letters word))))
      ((scan endings-4 (rv word))
       (let ((position (scan endings-4 (letters word))))
         (if (search "gu" (letters word) :from-end t :start2 (- position 2) :end2 position)
             (setf (fill-pointer (letters word)) (- position 1))
             (setf (fill-pointer (letters word)) position))))
      ((scan endings-5 (rv word))
       (setf (fill-pointer (letters word)) (scan endings-5 (letters word))))
      ((scan endings-6 (rv word))
       (let ((position (scan endings-6 (letters word))))
         (if (search "gu" (letters word) :from-end t :start2 (- position 2) :end2 position)
             (setf (fill-pointer (letters word)) (- position 1))
             (setf (fill-pointer (letters word)) position)))))))

(let ((endings-1
       (create-scanner
        (format nil "(~{~A~^|~})$" (list "os" "a" "o" "á" "í" "ó"))))
      (endings-2
       (create-scanner
        (format nil "(~{~A~^|~})$" (list "e" "é")))))
  (defmethod do-stem-step-3 ((language spanish) (word stemmed-word))
    (find-rv language word)
    (cond ((scan endings-1 (rv word))
           (let ((position (scan endings-1 (letters word) :start (rv-idx word))))
             (when position
               (setf (fill-pointer (letters word)) position))))
          ((scan endings-2 (rv word))
           (let ((position (scan endings-2 (letters word) :start (rv-idx word))))
             (when position
               (if (search "gu" (letters word) :from-end t :start2 (- position 2) :end2 position)
                   (setf (fill-pointer (letters word)) (- position 1))
                   (setf (fill-pointer (letters word)) position))))))))

(defmethod remove-acute-accents ((language spanish) (word stemmed-word))
  (loop for i from 0 below (word-length word) do
       (case (elt (letters word) i)
         (#\á  (setf (elt (letters word) i) #\a))
         (#\é  (setf (elt (letters word) i) #\e))
         (#\í  (setf (elt (letters word) i) #\i))
         (#\ó  (setf (elt (letters word) i) #\o))
         (#\ú  (setf (elt (letters word) i) #\u)))))

(defmethod stem ((language spanish) (string string) &key debug-p)
  (let ((word (initialize-stemmed-word language string)))
    (when debug-p
      (format t "ORIGINAL: ~A~%" word))
    (do-stem-step-0 language word)
    (when debug-p
      (format t "STEP-0: ~A~%" word))
    (let ((modified-p (do-stem-step-1 language word)))
      (when debug-p
        (format t "STEP-1: ~A~%" word))
      (unless modified-p
        (let ((modified-p (do-stem-step-2a language word)))
          (when debug-p
            (format t "STEP-2A: ~A~%" word))
          (unless modified-p
            (do-stem-step-2b language word)
            (when debug-p
              (format t "STEP-2B: ~A~%" word))))))
    (do-stem-step-3 language word)
    (when debug-p
      (format t "STEP-3: ~A~%" word))
    (remove-acute-accents language word)
    (when debug-p
      (format t "LAST: ~A~%" word))
    (letters word)))

(defmethod test-stemmer ((language spanish) file)
  (let ((mistakes nil) (total 0))
    (with-open-file (stream file
                            :element-type 'character
                            :external-format :utf-8) ;;:latin-1)
      (do ((line (read-line stream nil :eof) (read-line stream nil :eof)))
          ((eql line :eof))
        (destructuring-bind (&optional word stem) (split "\\s+" line)
          (when (and word stem)
            (incf total)
            (handler-case
                (let ((my-stem (stem language word :debug-p nil)))
                  (unless (equal my-stem stem)
                    (push word mistakes)
                    (format t "Improper stemming of '~A' should be '~A', got '~A'~%"
                            word stem my-stem)
                    ))
              (error (c)
                (format t "Improper stemming of '~A' should be '~A', got '~A'~%"
                        word stem c)
                ))))))
    (values (length mistakes)
            (format nil "~F" (/ (length mistakes) total))
            mistakes)))
