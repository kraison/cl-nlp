(in-package :nlp)

(defun make-random-directory (&optional (root "/var/tmp"))
  (let ((name nil))
    (loop until (and name
                     (not (cl-fad:directory-exists-p name))) do
         (setq name
               (format nil "~A/LISP-TEMP-~A-~A/"
                       root
                       (sb-posix:getpid)
                       (get-internal-real-time))))
    (ensure-directories-exist name)))

(defmacro with-temporary-directory ((directory) &body body)
  `(let ((,directory (make-random-directory)))
     (unwind-protect
          (progn ,@body)
       (cl-fad:delete-directory-and-files ,directory))))

(defun count-lines (file &key (external-format :utf-8))
  (with-open-file (stream file :external-format external-format)
    (do ((line (read-line stream nil :eof) (read-line stream nil :eof))
         (count 0 (1+ count)))
        ((eql line :eof) count)
      nil)))

(defun generate-random-numbers (count limit)
  (let ((table (make-hash-table)))
    (loop until (= count (hash-table-count table)) do
         (setf (gethash (random limit) table) t))
    ;;(alexandria:hash-table-keys table)))
    table))

(defclass confusion-matrix ()
  ((symbol-table :accessor symbol-table :initform (make-hash-table))
   (tags :accessor tags :initform nil :initarg :tags)
   (matrix :accessor matrix :initform (make-array '(13 13) :initial-element 0))))

(defmethod make-confusion-matrix ((language language))
  (let ((cm (make-instance 'confusion-matrix))
        (tags (part-of-speech-tags language)))
    (setf (tags cm) tags)
    (setf (matrix cm)
          (make-array (list (1+ (length tags)) (1+ (length tags)))
                      :initial-element 0))
    (dotimes (i (length tags))
      (setf (gethash (elt tags i) (symbol-table cm)) i))
    cm))

(defmethod record ((cm confusion-matrix) tag1 tag2)
  (let ((x (gethash tag1 (symbol-table cm) (- (array-dimension (matrix cm) 0) 1)))
        (y (gethash tag2 (symbol-table cm) (- (array-dimension (matrix cm) 1) 1))))
    (incf (aref (matrix cm) x y))))

(defmethod analyze-confusion-matrix ((cm confusion-matrix))
  (dotimes (i (hash-table-count (symbol-table cm)))
    (let ((tag (elt (tags cm) i)) (total 0))
      (format t "Confusion report for ~A:~%" tag)
      (dotimes (j (- (array-dimension (matrix cm) 1) 1))
        (incf total (aref (matrix cm) i j)))
      (if (zerop total)
          (format t "  Not used!~%")
          (format t "  Correct: ~F%~%"
                  (* 100 (/-safe (aref (matrix cm) i i) total))))
      (unless (zerop total)
        (dotimes (j (- (array-dimension (matrix cm) 1) 1))
          (unless (= i j)
            (unless (zerop (aref (matrix cm) i j))
              (format t "  ~A: ~F%~%"
                      (elt (tags cm) j)
                      (* 100 (/-safe (aref (matrix cm) i j) total))))))
        (format t "  NIL: ~F%~%"
                (* 100
                   (/-safe (aref (matrix cm)
                                 i
                                 (hash-table-count (symbol-table cm)))
                           total)))))
    (terpri)
    cm))

(defgeneric pos-match-p (language a b)
  (:method ((language language) a b)
    (or (eql a b)
        (and (noun-p language a) (noun-p language b))
        (and (verb-p language a) (verb-p language b))
        (and (adjective-p language a) (adjective-p language b))
        (and (determiner-p language a) (determiner-p language b))
        (and (pronoun-p language a) (pronoun-p language b))
        (and (adverb-p language a) (adverb-p language b)))))

(defun make-test-set (input-file test-file train-file test-lines external-format)
  (with-open-file (test-stream test-file
                               :direction :output
                               :if-does-not-exist :create
                               :external-format external-format
                               :element-type 'character)
    (with-open-file (train-stream train-file
                                  :direction :output
                                  :if-does-not-exist :create
                                  :external-format external-format
                                  :element-type 'character)
      (with-open-file (input-stream input-file
                                    :external-format external-format
                                    :element-type 'character)
        (do ((line (read-line input-stream nil :eof)
                   (read-line input-stream nil :eof))
             (count 0 (1+ count)))
            ((eql line :eof) count)
          (if (gethash count test-lines)
              (format test-stream "~A~%" line)
              (format train-stream "~A~%" line)))))))

(defun tag-test-set (language confusion-matrix test-file)
  (let ((tag-total-count 0)
        (tag-count-right 0)
        (tag-count-wrong 0))
    (let ((*language* language))
      (multiple-value-bind (sentences pos-seqs)
          (extract-tagged-sentences test-file)
        (map nil
             (lambda (sentence pos-seq)
               ;;(format t "Tagging '~{~A ~}'~%" sentence)
               (unless (every (lambda (tag)
                                (member tag (part-of-speech-tags language)))
                              pos-seq)
                 (error "Unknown tag in ~S!" pos-seq))
               (let ((guessed-tags (tag-sentence sentence)))
                 ;;(format t "B: ~{~S ~}~%" pos-seq)
                 ;;(format t "K: ~{~S ~}~%~%" guessed-tags)
                 ;;(when (null (y-or-n-p "Continue? "))
                 ;;  (return-from tag-test-set (values nil nil)))
                 (map nil
                      (lambda (p1 p2)
                        (incf tag-total-count)
                        (record confusion-matrix p1 p2)
                        (if (pos-match-p language p1 p2)
                            (incf tag-count-right)
                            (incf tag-count-wrong)))
                      pos-seq guessed-tags)))
             sentences pos-seqs)))
    (values tag-total-count tag-count-right tag-count-wrong)))

(defun do-cross-validation (language-fn file external-format &optional (cycles 10))
  (let ((scores nil) (confusion-matrix nil))
    (dotimes (cycle cycles)
      (format t "Starting cycle ~3,'0D~%" cycle)
      (with-temporary-directory (path)
        (let* ((train-file (format nil "~Atrain" path))
               (test-file (format nil "~Atest" path))
               (count (count-lines file :external-format external-format))
               (test-count (floor (* count 0.05)))
               (test-lines (generate-random-numbers test-count count)))
          (make-test-set file test-file train-file test-lines external-format)
          (let ((*language* (funcall language-fn :pos-train train-file)))
            (when (zerop cycle)
              (setq confusion-matrix (make-confusion-matrix *language*)))
            (format t "Starting tests~%")
            (multiple-value-bind (tag-total-count tag-count-right tag-count-wrong)
                (tag-test-set *language* confusion-matrix test-file)
              (declare (ignore tag-count-wrong))
              (format t "Cycle ~3,'0D: ~F% correct.~%~%"
                      cycle
                      (* 100 (/-safe tag-count-right tag-total-count)))
              (push (/-safe tag-count-right tag-total-count) scores)
              (close-language *language*))))))
    (format t "Average score: ~F%~%~%"
            (* 100 (/-safe (reduce '+ scores) (length scores))))
    (values confusion-matrix (/-safe (reduce '+ scores) (length scores)))))

(defvar *validation-defs* (make-hash-table))

(defmacro def-validation-test (language-name pos-file external-format)
  (let ((make-db-fn (intern (format nil "MAKE-~A-DB" language-name))))
    `(setf (gethash ',language-name *validation-defs*)
           (lambda ()
             (multiple-value-bind (confusion-matrix percent)
                 (do-cross-validation
                     (lambda (&key pos-train)
                       (,make-db-fn :pos-train pos-train :pos-lex ,pos-file))
                   ,pos-file ',external-format 10)
               (analyze-confusion-matrix confusion-matrix)
               (values confusion-matrix percent))))))

(defun train-and-test (language)
  (let ((fn (gethash language *validation-defs*)))
    (if fn
        (funcall fn)
        (error "Unknown language '~A'" language))))

(def-validation-test english "data/english-pos.txt" :utf-8)
(def-validation-test spanish "data/spanish-pos.txt" :latin-1)
(def-validation-test portuguese "data/portuguese-pos.txt" :utf-8)
(def-validation-test italian "data/italian-pos.txt" :utf-8)
(def-validation-test german "data/german-pos.txt" :utf-8)
(def-validation-test french "data/french-pos.txt" :utf-8)
