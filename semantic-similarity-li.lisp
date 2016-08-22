(in-package :nlp)

(alexandria:define-constant +e+ 2.718281828)
(alexandria:define-constant +default-alpha+ 0.2d0)
(alexandria:define-constant +default-beta+ 0.45d0)
(alexandria:define-constant +default-sigma+ 0.2d0)
(alexandria:define-constant +position-threshold+ 0.3d0)

(defvar *subsumer-depth-cache-li*
  (make-hash-table :test 'equalp :synchronized t :weakness nil))
(defvar *semantic-path-cache-li*
  (make-hash-table :test 'equalp :synchronized t :weakness nil))

(defun clear-caches-li ()
  (clrhash *subsumer-depth-cache-li*)
  (clrhash *semantic-path-cache-li*))

(defun cosine-similarity (s1 s2)
  "Compute the cosine similarity of two sequences."
  (let ((numerator (sum (map 'list #'* s1 s2))))
    (/-double numerator
              (* (sqrt (sum (map 'list #'square s1)))
                 (sqrt (sum (map 'list #'square s2)))))))

(defun euclidean-distance (t1 t2)
  "Compute the euclidean distance between two sequences"
  (sqrt (sum (map 'list (lambda (x y) (expt (- x y) 2)) t1 t2))))

(defun word-order-distance (s1 s2)
  (/-double
   (sqrt (sum (map 'list 'square (map 'list (lambda (x y) (- x y)) s1 s2))))
   (sqrt (sum (map 'list 'square (map 'list (lambda (x y) (+ x y)) s1 s2))))))

(defun word-order-similarity (s1 s2)
  (- 1 (word-order-distance s1 s2)))

(defmethod calculate-subsumer-depth ((subsumer synset) &key &allow-other-keys)
  (if subsumer
      (or (gethash subsumer *subsumer-depth-cache-li*)
          (let ((parents (semantic-parents subsumer))
                (depth 0)
                (memory (make-hash-table :test 'equalp)))
            (loop until (null parents) do
                 (incf depth)
                 (let ((next-parents nil))
                   (dolist (parent parents)
                     (unless (gethash parent memory)
                       (setf (gethash parent memory) t)
                       (setq next-parents
                             (append next-parents
                                     (semantic-parents parent)))))
                   (setq parents (remove-duplicates next-parents))))
            (setf (gethash subsumer *subsumer-depth-cache-li*) depth)))
      0))

(defmethod calculate-subsumer-depth ((subsumer null) &key &allow-other-keys)
  0)

(defmethod calculate-subsumer-depth ((subsumer string) &key pos &allow-other-keys)
  (let ((key (cons subsumer pos)))
    (or (gethash key *subsumer-depth-cache-li*)
        (let ((parents (semantic-parents subsumer :pos pos))
              (depth 0)
              (memory (make-hash-table :test 'equalp)))
          (loop until (null parents) do
               (incf depth)
               (let ((next-parents nil))
                 (dolist (parent parents)
                   (unless (gethash parent memory)
                     (setf (gethash parent memory) t)
                     (setq next-parents
                           (append next-parents
                                   (semantic-parents parent)))))
                 (setq parents (remove-duplicates next-parents))))
          (setf (gethash key *subsumer-depth-cache-li*) depth)))))

(defun climb-semantic-tree (word-1 word-2 &optional pos)
  (declare (ignore pos))
  (let ((levels-1 (list (synonyms word-1)))
        (levels-2 (list (synonyms word-2)))
        (seen-1 (make-hash-table))
        (seen-2 (make-hash-table)))
    (when (and (first levels-1) (first levels-2))
      (map nil (lambda (s1 s2)
                 (setf (gethash s1 seen-1) t)
                 (setf (gethash s2 seen-2) t))
           (first levels-1) (first levels-2))
      (loop for i from 0 to 25 do
           (let ((next-1 (remove-if (lambda (s)
                                      (gethash s seen-1))
                                    (union-all (mapcar 'semantic-parents
                                                       (elt levels-1 i)))))
                 (next-2 (remove-if (lambda (s)
                                      (gethash s seen-2))
                                    (union-all (mapcar 'semantic-parents
                                                       (elt levels-2 i))))))
             (when (and (null next-1) (null next-2))
               (return-from climb-semantic-tree (values 100 100)))
             (map nil (lambda (s1 s2)
                        (setf (gethash s1 seen-1) t)
                        (setf (gethash s2 seen-2) t))
                  next-1 next-2)
             (loop for j from 0 to i do
                  (let ((intersection-1 (intersection next-1 (elt levels-2 j)))
                        (intersection-2 (intersection next-2 (elt levels-1 j))))
                    (cond (intersection-1
                           (return-from climb-semantic-tree
                             (values (+ 2 i j)
                                     (apply 'max
                                            (mapcar 'calculate-subsumer-depth
                                                    intersection-1)))))
                          (intersection-2
                           (return-from climb-semantic-tree
                             (values (+ 2 i j)
                                     (apply 'max
                                            (mapcar 'calculate-subsumer-depth
                                                    intersection-2)))))
                          (t
                           (setq levels-1 (append levels-1 (list next-1))
                                 levels-2 (append levels-2 (list next-2)))))))
             (let ((intersection-3 (intersection next-1 next-2)))
               (when intersection-3
                 (return-from climb-semantic-tree
                   (values (+ 2 i i)
                           (apply 'max
                                  (mapcar 'calculate-subsumer-depth
                                          intersection-3))))))))
      (values 100 100))))

(defun calculate-semantic-path (word-1 word-2 pos)
  (declare (ignore pos))
  (let ((key (if (string> word-1 word-2)
                 (list word-1 word-2)
                 (list word-2 word-1))))
    (multiple-value-bind (path-metrics path-present-p)
        (gethash key *semantic-path-cache-li*)
      (if path-present-p
          (values (car path-metrics) (cdr path-metrics))
          (let ((length nil) (depth nil))
            (if (or (string-equal word-1 word-2)
                    (string-equal (porter-stemmer:stem word-1)
                                  (porter-stemmer:stem word-2)))
                (setq length 0
                      depth 100)
                (let* ((word-set-1 (synonyms word-1 :return-type :words))
                       (word-set-2 (synonyms word-2 :return-type :words)))
                  (cond ((or (null word-set-1) (null word-set-2))
                         t)
                        ((or (find word-1 word-set-2 :test 'string-equal)
                             (find word-2 word-set-1 :test 'string-equal))
                         (setq length 0
                               depth (max (calculate-subsumer-depth word-1)
                                          (calculate-subsumer-depth word-2))))
                        ((multiple-value-bind (i count)
                             (counting-intersection word-set-1 word-set-2)
                           (declare (ignore i))
                           (> count 0))
                         (setq length 1
                               depth (max (calculate-subsumer-depth word-1)
                                          (calculate-subsumer-depth word-2))))
                        (t
                         (multiple-value-bind (l d)
                             (climb-semantic-tree word-1 word-2)
                           (setq length l
                                 depth d))))))
            (setf (gethash key *semantic-path-cache-li*) (cons length depth))
            (values length depth))))))

(defun calc-word-similarity (path-length path-depth
                             &optional (alpha +default-alpha+)
                               (beta +default-beta+))
  (* (exp (- (* alpha path-length)))
     (/ (- (exp (* beta path-depth))
           (exp (- (* beta path-depth))))
        (+ (exp (* beta path-depth))
           (exp (- (* beta path-depth)))))))

(defun word-similarity (word-1 word-2 pos
                        &key (alpha +default-alpha+) (beta +default-beta+))
  (multiple-value-bind (path-length path-depth)
      (calculate-semantic-path word-1 word-2 pos)
    (if (and path-length path-depth)
        (calc-word-similarity path-length path-depth alpha beta)
        0)))

(defun compute-semantic-score (feature-word analysis)
  (let ((feature-information (information-content feature-word)))
    (if (member feature-word
                (apply 'append (mapcar 'second analysis)) :test 'string-equal)
        (* 1 feature-information feature-information)
        (let ((score 0)
              (most-similar-word nil))
          (dolist (sentence analysis)
            (dotimes (i (length (first sentence)))
              (let ((pos (elt (first sentence) i))
                    (word (elt (second sentence) i)))
                (unless (eql pos '|.|)
                  (let ((this-score (word-similarity word feature-word pos)))
                    (when (> this-score score)
                      (setq score this-score
                            most-similar-word word)))))))
          (if (>= score +default-sigma+)
              (* score
                 feature-information
                 (if most-similar-word
                     (information-content most-similar-word)
                     feature-information))
              0)))))

(defun find-most-similar-word (feature analysis
                               &key (threshold +position-threshold+))
  (let ((position nil) (score 0))
    (dolist (sentence analysis)
      (dotimes (i (length (first sentence)))
        (let ((pos (elt (first sentence) i))
              (word (elt (second sentence) i)))
          (let ((similarity (word-similarity word feature pos)))
            (when (> similarity score)
              (setq score similarity
                    position i))))))
    (if (and (numberp position)
             (> score threshold))
        (1+ position)
        0)))

(defun set-word-position (word analysis position-vector feature-list)
  (let ((words (remove-if 'punctuation-p (flatten (mapcar 'second analysis))))
        (feature-position (position word feature-list :test 'string-equal)))
    (let ((position (position word words :test 'string-equal)))
      (if (numberp position)
          ;; this algorithm wants arrays counted from 1, not 0
          (setf (svref position-vector feature-position)
                (1+ position))
          (setf (svref position-vector feature-position)
                (find-most-similar-word word analysis))))))

(defun make-semantic-vectors-li (analysis-1 analysis-2)
  (let* ((feature-list (remove-if 'punctuation-p
                                  (union (flatten (mapcar 'second analysis-1))
                                         (flatten (mapcar 'second analysis-2))
                                         :test 'string-equal)))
         (feature-count (length feature-list))
         (semantic-vector-1 (make-numeric-vector feature-count))
         (semantic-vector-2 (make-numeric-vector feature-count))
         (position-vector-1 (make-numeric-vector feature-count))
         (position-vector-2 (make-numeric-vector feature-count)))
    (dotimes (i feature-count)
      (let ((feature (elt feature-list i)))
        (setf (svref semantic-vector-1 i)
              (compute-semantic-score feature analysis-1))
        (setf (svref semantic-vector-2 i)
              (compute-semantic-score feature analysis-2))
        (set-word-position feature analysis-1 position-vector-1 feature-list)
        (set-word-position feature analysis-2 position-vector-2 feature-list)))
    (values semantic-vector-1
            semantic-vector-2
            position-vector-1
            position-vector-2
            feature-list)))

(defun semantic-similarity-li (analysis-1 analysis-2 &key (semantic-weight 0.85d0))
  (multiple-value-bind
        (semantic-vector-1 semantic-vector-2 position-vector-1 position-vector-2
                           feature-list)
      (make-semantic-vectors-li analysis-1 analysis-2)
    (values (+ (* semantic-weight
                  (cosine-similarity semantic-vector-1 semantic-vector-2))
               (* (- 1 semantic-weight)
                  (word-order-similarity position-vector-1 position-vector-2)))
            (cosine-similarity semantic-vector-1 semantic-vector-2)
            (word-order-similarity position-vector-1 position-vector-2)
            feature-list
            semantic-vector-1
            semantic-vector-2
            position-vector-1
            position-vector-2)))

(defun semantic-distance-li (analysis-1 analysis-2 &key (semantic-weight 0.85d0))
  (multiple-value-bind
        (final-similarity semantic-similarity word-order-similarity features
                          semantic-vector-1 semantic-vector-2 position-vector-1
                          position-vector-2)
      (semantic-similarity-li analysis-1
                              analysis-2
                              :semantic-weight semantic-weight)
    (values (- 1 final-similarity)
            (- 1 semantic-similarity)
            (- 1 word-order-similarity)
            features
            semantic-vector-1
            semantic-vector-2
            position-vector-1
            position-vector-2)))

(defun negative-squared-euclidean-distance (t1 t2)
  "Compute the euclidean distance between two sequences,
-( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )"
  (- (sum (map 'list (lambda (x y) (expt (- x y) 2)) t1 t2))))

(defun word-order-squared-distance (s1 s2)
  (- (/-double
      (sum (map 'list 'square (map 'list (lambda (x y) (- x y)) s1 s2)))
      (sum (map 'list 'square (map 'list (lambda (x y) (+ x y)) s1 s2))))))

(defun negative-squared-semantic-distance-li (analysis-1 analysis-2
                                              &key (semantic-weight 0.85d0))
  (multiple-value-bind
        (semantic-vector-1 semantic-vector-2 position-vector-1 position-vector-2
                           feature-list)
      (make-semantic-vectors-li analysis-1 analysis-2)
    (values (+ (* semantic-weight
                  (negative-squared-euclidean-distance semantic-vector-1
                                                       semantic-vector-2))
               (* (- 1 semantic-weight)
                  (word-order-squared-distance position-vector-1
                                               position-vector-2)))
            (negative-squared-euclidean-distance semantic-vector-1
                                                 semantic-vector-2)
            (word-order-squared-distance position-vector-1 position-vector-2)
            feature-list
            semantic-vector-1
            semantic-vector-2
            position-vector-1
            position-vector-2)))
