(in-package :nlp)

(defvar *pos-fuzz-table* (make-hash-table))

(defparameter *penn-tags*
  '(CC CD DT EX FW IN JJ JJR JJS LS MD NN NNS NNP NNPS PDT POS PRP PRP$ RB RBR RBS RP SYM
    TO UH VB VBD VBG VBN VBP VBZ WDT WP WP$ WRB))

(defvar *pos-fuzz-vec*
  #2A((0 1 1 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 0 1 1 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (2 1 2 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.4 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 0 0.1 0.1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 0.1 0 0.2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 0.1 0.2 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 0 1 0.1 1 1 1 0.4 1 1 1 1 1 1 1 1 3 3 3 3 3 3 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 0 1 0.1 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 0.1 1 0 1 1 1 0.4 1 1 1 1 1 1 1 1 3 1 3 3 3 3 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 0.1 1 0 1 1 1 1 1 1 1 1 1 1 1 3 1 3 3 3 3 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0.6 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.2
         1)
      (1 1 1 1 1 1 1 1 1 1 1 0.4 1 0.4 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.6 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.2
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0.1 0.1 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.1 0 0.2 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.1 0.2 0 1 1 1 1 1 1 1 1 1 1 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 0.4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0.2 0.1 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 0.2 1 1 1 0.1 0.2 1 1
         1 1)
      (1 1 1 1 1 1 1 1 1 1 1 3 3 3 3 1 1 1 1 1 1 1 1 1 1 1 0.1 1 1 1 0.2 0 1 1 1
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0.2 1 0.2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
         1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0)))

(defun pos-distance (t1 t2)
  "Find the distance between 2 POS tags"
  (let ((t1 (etypecase t1
              (string (intern t1))
              (symbol (intern (symbol-name t1)))))
        (t2 (etypecase t2
              (string (intern t2))
              (symbol (intern (symbol-name t2))))))
    (or (gethash t2 (gethash t1 *pos-fuzz-table*))
        (gethash t1 (gethash t2 *pos-fuzz-table*))
        0)))

(defun tree-similarity (t1 t2)
  "Compute the similarity between two vectors of POS tags"
  (let ((*package* (find-package "NLP")))
    (let ((t1 (etypecase t1
                (string (read-from-string t1))
                (cons t1)))
          (t2 (etypecase t2
                (string (read-from-string t2))
                (cons t2))))
      (cond ((= (length t1) (length t2))
             (mapcar (lambda (p1 p2)
                       (pos-similarity p1 p2))
                     t1 t2))
            (t 0)))))

(defun pos-insert-cost (s)
  "Insert cost function for POS tags"
  (case s
    ((CC CD EX FW IN LS NN NNP NNPS NNS PDT POS PRP PRP$ TO SYM VB VBD VBG VBN VBP VBZ
         WDT WP WP$ WRB) 1)
    (DT  0.5)
    (JJ  0.1)
    (JJR 0.3)
    (JJS 0.3)
    (MD  0.8)
    (RB  0.1)
    (RBR 0.3)
    (RBS 0.3)
    (RP  0.9)
    (UH  0.1)))

(defun pos-delete-cost (s)
  "Delete cost function for POS tags"
  (case s
    ((CC CD EX FW IN LS NN NNP NNPS NNS PDT POS PRP PRP$ SYM TO VB VBD VBG VBN VBP VBZ
         WDT WP WP$ WRB) 1)
    (DT  0.5)
    (JJ  0.1)
    (JJR 0.3)
    (JJS 0.3)
    (MD  0.8)
    (RB  0.1)
    (RBR 0.3)
    (RBS 0.3)
    (RP  0.9)
    (UH  0.1)))

(defun pos-edit-distance (s1 s2 &key (insert-cost 1) (delete-cost 1))
  "Compute the edit distance between two POS vectors"
  (let ((*package* (find-package "NLP")))
    (let ((s1 (etypecase s1
                (string (read-from-string (regex-replace-all "(,|\\.|\\:)" s1 "")))
                (cons s1)))
          (s2 (etypecase s2
                (string (read-from-string (regex-replace-all "(,|\\.|\\:)" s2 "")))
                (cons s2))))
      (let* ((s1-length (length s1))
             (s2-length (length s2))
             (distance (make-array `(,(1+ s1-length) ,(1+ s2-length))
                                   :initial-element 0)))
        (loop for i from 1 to s1-length do
             (setf (aref distance i 0) (+ (aref distance (1- i) 0) insert-cost)))
        (loop for j from 1 to s2-length do
             (setf (aref distance 0 j) (+ (aref distance 0 (1- j)) delete-cost)))
        (loop for i from 1 to s1-length do
             (loop for j from 1 to s2-length do
                  (setf (aref distance i j)
                        (min (+ (aref distance (1- i) j) (pos-insert-cost (elt s2 (1- j))))
                             (+ (aref distance i (1- j)) (pos-delete-cost (elt s2 (1- j))))
                             (+ (aref distance (1- i) (1- j))
                                (if (eql (elt s1 (1- i)) (elt s2 (1- j)))
                                    0
                                    (pos-distance (elt s1 (1- i)) (elt s2 (1- j)))))))))
        (values (aref distance s1-length s2-length) distance)))))
        ;;(aref distance s1-length s2-length)))))

(defun make-pos-fuzz (&key manual)
  "Populate the POS distance matrix and hash table index"
  (dolist (tag *penn-tags*)
    (setf (gethash tag *pos-fuzz-table*) (make-hash-table)))
  (dotimes (i (length *penn-tags*))
    (loop for j from i to (1- (length *penn-tags*)) do
         (let ((p (if manual
                      (progn
                        (format t "SIM(~A,~A) = " (elt *penn-tags* i) (elt *penn-tags* j))
                        (read))
                      (aref *pos-fuzz-vec* i j))))
           (setf (aref *pos-fuzz-vec* i j) p)
           (setf (aref *pos-fuzz-vec* j i) p)
           (setf (gethash (nth i *penn-tags*) (gethash (nth j *penn-tags*) *pos-fuzz-table*)) p)
           (setf (gethash (nth j *penn-tags*) (gethash (nth i *penn-tags*) *pos-fuzz-table*)) p))))
  (values *penn-tags* *pos-fuzz-vec*))

(defun set-pos-fuzz (t1 t2 p)
  "Set the cost for substituting one POS for another"
  (let ((i (position t1 *penn-tags*)) (j (position t2 *penn-tags*)))
    (setf (aref *pos-fuzz-vec* i j) p)
    (setf (aref *pos-fuzz-vec* j i) p)
    (setf (gethash (nth i *penn-tags*) (gethash (nth j *penn-tags*) *pos-fuzz-table*)) p)
    (setf (gethash (nth j *penn-tags*) (gethash (nth i *penn-tags*) *pos-fuzz-table*)) p)))

(defun dump-pos-table ()
  (maphash (lambda (k v)
             (maphash (lambda (k1 v1)
                        (format t "~A / ~A => ~A~%" k k1 v1))
                      v))
           *pos-fuzz-table*))
