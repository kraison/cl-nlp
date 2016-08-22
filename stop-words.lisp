(in-package :nlp)

(defmethod load-stop-words ((language language) file &key (external-format :utf-8))
  "Load stop words from a file, one stop word per line."
  (let ((table (make-hash-table :test 'equalp)))
    (with-open-file (in file
                        :element-type 'character
                        :external-format external-format)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (setf (gethash line table) t)))
    (setf (stop-words language) table)))

(defmethod add-stop-word ((language language) (word string))
  (setf (gethash word (stop-words language)) t))

(defmethod remove-stop-word ((language language) (word string))
  (remhash word (stop-words language)))

(defmethod stop-word-p ((language language) (word string))
  "Is word a stop-word?"
  (if (hash-table-p (stop-words language))
      (gethash word (stop-words language))
      nil))

(defmethod remove-stop-words ((language language) (string string))
  "Remove stop words from a string"
  (format nil "~{~A~^ ~}"
          (remove-if (lambda (word)
                       (stop-word-p language word))
                     (cl-ppcre:split "\\s+" string))))

(defmethod remove-stop-words ((language language) (word-seq sequence))
  "Remove stop words from a sequence of words"
  (remove-if (lambda (word)
               (stop-word-p language word))
             word-seq))

(defmethod wildcard-stop-words ((language language) (string string))
  "Wildcard stop words in a string for use in regexes"
  (format nil "~{~A~^ ~}" (mapcar (lambda (word)
                                    (if (stop-word-p language word)
                                        "\\w+"
                                        word))
                                  (cl-ppcre:split "\\s+" string))))

(defmethod wildcard-stop-words ((language language) (word-seq sequence))
  "Wildcard stop words in a sequence of strings for use in regexes"
  (map (type-of word-seq)
       (lambda (word)
         (if (stop-word-p language word)
             "\\w+"
             word))
       word-seq))
