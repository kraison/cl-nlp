(in-package #:nlp)

(defstruct agenda edge-list (memory (make-hash-table :test 'equalp)))

(defun add-to-agenda (agenda edge)
  "Add an edge to the agenda"
  (unless (gethash edge (agenda-memory agenda))
    (pushnew edge (agenda-edge-list agenda) :test 'edge-equalp)))

(defun pop-agenda (agenda)
  "Get next edge off of the agenda"
  (let ((edge (pop (agenda-edge-list agenda))))
    (setf (gethash edge (agenda-memory agenda)) t)
    edge))

(defun pcp-add-edge-to-chart (chart edge)
  "Add an edge to the chart"
  (unless (member edge (elt (chart-edge-vec chart) (right-vertex edge))
                  :test 'edge-equalp)
    (push edge (elt (chart-edge-vec chart) (right-vertex edge)))))

(defun pcp-parse (text)
  "Probabilistic parser based on Klein & Manning 2001"
  (multiple-value-bind (words tags chart f-agenda e-agenda)
      (pcp-initialize text)
    (while (not (null (agenda-edge-list f-agenda)))
      ;; First, go through the exploration agenda
      (while (not (null (agenda-edge-list e-agenda)))
        (let ((traversal (pop (agenda-edge-list e-agenda))))
          (explore-traversal f-agenda traversal)))
      ;; Then go through the finishing agenda
      (setf (agenda-edge-list f-agenda)
            (sort (agenda-edge-list f-agenda) '> :key 'probability))
      (let ((edge (pop-agenda f-agenda)))
        (finish-edge chart words f-agenda e-agenda edge))
      (while (not (null (agenda-edge-list e-agenda)))
        (let ((traversal (pop (agenda-edge-list e-agenda))))
          (explore-traversal f-agenda traversal)))
      (dolist (edge (elt (chart-edge-vec chart) (length words)))
        (when (and (eq :start (label edge))
                   (= 0 (left-vertex edge))
                   (= (length words) (right-vertex edge)))
          (return-from pcp-parse
            (values (p-find-trees-in-chart chart words) tags)))))
    (values (p-find-trees-in-chart chart words) tags)))

(defun pcp-initialize (text)
  "Initialize the words on text"
  ;;(multiple-value-bind (pos-with-p words) (possible-tags text :p? t)
  (multiple-value-bind (tags words) (tag-sentence text)
    (setq tags (mapcar (lambda (tag word)
                         (if (null tag)
                             ;; FIXME: need language object
                             (lookup-pos *language* word)
                             (list tag)))
                       tags words))
    (let ((chart (make-chart :edge-vec
                             (make-array (1+ (length words))
                                         :initial-element nil)))
          (f-agenda (make-agenda :edge-list nil))
          (e-agenda (make-agenda :edge-list nil)))
      (dotimes (i (length words))
        (let ((word (nth i words)) (tags (nth i tags)))
          (dolist (pos tags)
            (log:debug "Adding edge for ~A/~A" word pos)
            (let ((edge (make-chart-edge :left-vertex i
                                         :right-vertex (1+ i)
                                         :label pos
                                         :edge-word word
                                         :to-find nil
                                         :found (list word)
                                         :probability 1)))
              (discover-edge f-agenda edge)))))
      (values words tags chart f-agenda e-agenda))))

(defun explore-traversal (f-agenda traversal)
  (discover-edge f-agenda (third traversal)))

(defun discover-edge (f-agenda edge)
  (add-to-agenda f-agenda edge))

(defun pcp-fundamental-rule (chart e-agenda edge)
  "Fundamental rule for PCP"
  (if (to-find edge)
      ;; EDGE is an active edge
      (map nil
           (lambda (edge-list)
             (dolist (p-edge edge-list)
               (when (and (null (to-find p-edge))
                          (= (right-vertex edge) (left-vertex p-edge))
                          (eq (label p-edge) (first (to-find edge))))
                 (let ((new-edge (make-chart-edge
                                  :left-vertex (left-vertex edge)
                                  :right-vertex (right-vertex p-edge)
                                  :label (label edge)
                                  :to-find (rest (to-find edge))
                                  :found (cons p-edge (found edge)))))
                   (if (every 'chart-edge-p (found edge))
                       (setf (probability new-edge)
                             (handler-case
                                 (* (probability edge)
                                    (reduce '* (found edge)
                                            :key 'probability))
                               (error (c)
                                 (declare (ignore c))
                                 0)))
                       (setf (probability new-edge)
                             (probability edge)))
                   (push (list edge p-edge new-edge)
                         (agenda-edge-list e-agenda))))))
           (chart-edge-vec chart))
      (map nil
           (lambda (edge-list)
             (dolist (a-edge edge-list)
               (when (and (to-find a-edge)
                          (or (= (right-vertex edge) (right-vertex a-edge))
                              (= (left-vertex edge) (right-vertex a-edge)))
                          (eq (label edge) (first (to-find a-edge))))
                 (let ((new-edge (make-chart-edge
                                  :left-vertex (left-vertex a-edge)
                                  :right-vertex (right-vertex edge)
                                  :label (label a-edge)
                                  :to-find (rest (to-find a-edge))
                                  :found (cons edge (found a-edge)))))
                   (if (every 'chart-edge-p (found a-edge))
                       (setf (probability new-edge)
                             (handler-case
                                 (* (probability a-edge)
                                    (reduce '* (found a-edge)
                                            :key 'probability))
                               (error (c)
                                 (declare (ignore c))
                                 0)))
                       (setf (probability new-edge)
                             (probability a-edge)))
                   (push (list a-edge edge new-edge)
                         (agenda-edge-list e-agenda))))))
           (chart-edge-vec chart))))

(defun pcp-bottom-up-rule (f-agenda edge)
  "Bottom up rule for PCP"
  (let* ((pos (label edge))
         (productions (lookup-pos-productions pos)))
    (dolist (s productions)
      (let ((p-hash (gethash s (pcfg *language*))))
        (maphash (lambda (production p)
                   (if (listp production)
                       (when (eq (first production) pos)
                         (discover-edge
                          f-agenda
                          (make-chart-edge :left-vertex (left-vertex edge)
                                           :right-vertex (left-vertex edge)
                                           :label s
                                           :to-find production
                                           :found nil
                                           :probability p)))
                       (when (eq production pos)
                         (discover-edge
                          f-agenda
                          (make-chart-edge :left-vertex (left-vertex edge)
                                           :right-vertex (left-vertex edge)
                                           :label s
                                           :to-find (list production)
                                           :found nil
                                           :probability p)))))
                 p-hash)))))

(defun pcp-top-down-rule (f-agenda words edge)
  "Top down rule for PCP"
  (when (to-find edge)
    (dolist (pos (to-find edge))
      (let ((table-or-word (gethash pos (pcfg *language*))))
        (if (hash-table-p table-or-word)
            (maphash (lambda (prod p)
                       (when (or (listp prod)
                                 (member (symbol-name prod) words
                                         :test 'equal))
                         (discover-edge f-agenda
                                        (make-chart-edge
                                         :left-vertex 0
                                         :right-vertex 0
                                         :label pos
                                         :to-find (if (listp prod)
                                                      prod
                                                      (list prod))
                                         :probability p))))
                     table-or-word)
            (log:debug "pcp-top-down-rule got ~A when looking up ~A"
                       table-or-word pos))))))

(defun finish-edge (chart words f-agenda e-agenda edge)
  (declare (ignore words))
  (pcp-add-edge-to-chart chart edge)
  (pcp-fundamental-rule chart e-agenda edge)
  ;;(pcp-top-down-rule f-agenda words edge)
  (pcp-bottom-up-rule f-agenda edge)
  )
