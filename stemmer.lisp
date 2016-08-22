(in-package :nlp)

;;; FIXME: the handling of character encoding is pretty ugly here since we are
;;;        mapping to an external C library.  This needs some cleanup.

(defun lisp-encoding-to-stemmer (encoding)
  (case encoding
    (:utf-8 "UTF_8")
    (:latin-1 "ISO_8859_1")
    (otherwise "UTF_8")))

(defun do-stemming (language-name encoding word)
  (let ((stemmer (sb_stemmer_new language-name encoding)))
    (unwind-protect
         (cffi:with-foreign-string (input-string word)
           (let ((foreign-string
                  (sb_stemmer_stem stemmer input-string (length word))))
             (cffi:foreign-string-to-lisp foreign-string
                                          :encoding
                                          (cond ((equalp encoding "ISO_8859_1")
                                                 :latin-1)
                                                (t
                                                 :utf-8)))))
      (sb_stemmer_delete stemmer))))

(defgeneric stem (language word &key encoding))

(defmethod stem ((language language) (word string) &key encoding)
  (stem (name language)
        word
        :encoding (or encoding
                      (lisp-encoding-to-stemmer (default-encoding language)))))

(defmethod stem ((language string) (word string) &key (encoding "UTF_8"))
  (handler-case
      (do-stemming language encoding word)
    (babel-encodings:end-of-input-in-character (c)
      (declare (ignore c))
      ;; wrong encoding;  try another one!
      (handler-case
          (cond ((equalp encoding "UTF_8")
                 (do-stemming language "ISO_8859_1" word))
                ((equalp encoding "ISO_8859_1")
                 (do-stemming language "UTF_8" word))
                (t
                 nil))
        (error (c)
          (declare (ignore c))
          nil)))))
