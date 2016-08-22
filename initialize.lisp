(in-package #:nlp)

(defmethod freeze-nlp ((language language) &optional file)
  (let ((file (or file (format nil "~A.dat" (name language)))))
    (let ((lexicon-dbm (lexicon-dbm language))
          (plexicon-dbm (plexicon-dbm language))
          (observations-dbm (observations-dbm language))
          (word-occurrence-dbm (word-occurrence-dbm language)))
      (setf (lexicon-dbm language) nil
            (plexicon-dbm language) nil
            (observations-dbm language) nil
            (word-occurrence-dbm language) nil)
      (cl-store:store language file)
      (setf (lexicon-dbm language) lexicon-dbm
            (plexicon-dbm language) plexicon-dbm
            (observations-dbm language) observations-dbm
            (word-occurrence-dbm language) word-occurrence-dbm)
      language)))

(defun thaw-nlp (file &key data-dir)
  (let ((language (cl-store:restore file)))
    (open-lexicon-dbm language :data-dir data-dir)
    (open-plexicon-dbm language :data-dir data-dir)
    (open-observations-dbm language :data-dir data-dir)
    (open-word-occurrence-dbm language :data-dir data-dir)
    (add-language language)))

(defmacro maybe-profile ((&body body))
  `(if profile-p
       (time (progn ,body))
       (progn
         ,body)))
