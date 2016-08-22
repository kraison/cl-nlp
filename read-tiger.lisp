(in-package :nlp)

(defun convert-tiger-conll-file (in-file out-file)
  (with-open-file (stream out-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type 'character
                          :external-format :utf-8)
    (map-conll-corpus (lambda (sentence pos-seq)
                        (dotimes (i (length sentence))
                          (format stream "~A/~A" (elt sentence i) (elt pos-seq i))
                          (if (= i (- (length sentence) 1))
                              (terpri stream)
                              (format stream " "))))
                      in-file)))

(defun find-dom-element (name dom)
  (find name dom
        :key (lambda (e) (and (consp e) (first e)))
        :test 'equalp))

(defun index-terminals (index terminals)
  (delete-if
   'null
   (mapcar (lambda (terminal)
             (when (and (consp terminal)
                        (equalp (first terminal) "t"))
               (let ((items (second terminal)))
                 (setf (gethash (second (find-dom-element "id" items)) index)
                       (list (intern (string-upcase
                                      (second (find-dom-element "pos" items)))
                                     :keyword)
                             (second (find-dom-element "word" items)))))))
           terminals)))

(defun index-nonterminals (index nonterminals)
  (delete-if
   'null
   (mapcar
    (lambda (node)
      (when (and (consp node) (equalp (first node) "nt"))
        (let ((category (intern (second (first (second node))) :keyword))
              (id (second (second (second node)))))
          (setf (gethash id index)
                (list category
                      (delete-if
                       'null
                       (mapcar
                        (lambda (edge)
                          (when (and (consp edge)
                                     (equalp "edge" (first edge)))
                            (list (intern (second (second (second edge))) :keyword)
                                  (second (first (second edge))))))
                        node)))))))
    nonterminals)))

(defun convert-tiger-xml-file (in-file out-file)
  (let* ((dom (cxml:parse-file in-file (cxml-xmls:make-xmls-builder)))
         (body (find-dom-element "body" dom)))
    (with-open-file (stream out-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type 'character
                            :external-format :utf-8)
      (dolist (node body)
        (when (and (consp node) (equalp (first node) "s"))
          (let ((graph (find-dom-element "graph" node))
                (index (make-hash-table :test 'equalp)))
            (let ((terminals
                   (index-terminals index
                                    (find-dom-element "terminals" graph)))
                  (nonterminals
                   (index-nonterminals index
                                       (find-dom-element "nonterminals" graph))))
              (let ((used-nodes nil))
                (dolist (nt nonterminals)
                  (loop for i from 0 below (length (second nt)) do
                       (let ((id (second (nth i (second nt)))))
                         (when id
                           (let ((sub-node (gethash id index)))
                             (push sub-node used-nodes)
                             (setf (second (nth i (second nt))) sub-node))))))
                (let ((tree
                       (set-difference (append terminals nonterminals)
                                       used-nodes
                                       :test 'equalp)))
                  (when tree
                    (write tree :stream stream)
                    (terpri stream)))))))))))

(defun flatten-tiger-phrase-tree (tree)
  (let ((leaf-phrases nil))
    (labels ((dfs-helper (this-node phrase-type)
               (cond ((and (consp this-node)
                           (eql :NP (first this-node))
                           (= 2 (length this-node))
                           (every (lambda (child)
                                    (and (consp child)
                                         (= 2 (length child))
                                         (symbolp (first child))
                                         (consp (second child))
                                         (every 'atom (second child))))
                                  (second this-node)))
                      (dolist (child (second this-node))
                        (push (list :NP (first (second child))) leaf-phrases)))
                     ((and (consp this-node)
                           (= 2 (length this-node))
                           (atom (first this-node))
                           (atom (second this-node)))
                      (push (list phrase-type (first this-node)) leaf-phrases))
                     ((and (consp this-node)
                           (atom (first this-node))
                           (every 'consp (rest this-node))
                           (every (lambda (c) (= 2 (length c))) (rest this-node)))
                      (dolist (phrase (rest this-node))
                        (dfs-helper phrase (first this-node))))
                     ((consp this-node)
                      (dolist (child this-node)
                        (dfs-helper child (first this-node)))))))
      (dfs-helper tree nil))
    (nreverse leaf-phrases)))
