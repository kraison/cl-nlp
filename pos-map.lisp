(in-package :nlp)

(defvar *pos-maps* (make-hash-table :test 'eql))

(defmacro def-pos-map (name pairs)
  `(let ((map
          (list
           ,@(mapcar (lambda (pair)
                       `(cons
                         ,(first pair)
                         ,(if (symbolp (second pair))
                              `(intern (symbol-name ',(second pair)) :keyword)
                              `(intern ,(second pair) :keyword))))
                     pairs))))
     (dolist (pair map)
       (unless (valid-pos-tag-p (cdr pair))
         (error "Invalid tags in definition of ~A: ~A" ',name pair)))
     (setf (gethash ,name *pos-maps*) map)))

(defgeneric lookup-generic-pos (name pos &key test))

(defmethod lookup-generic-pos (name pos &key (test 'equal))
  (let ((alist (gethash name *pos-maps*)))
    (when alist
      (cdr (assoc pos alist :test test)))))

(defgeneric lookup-specific-pos (name pos &key test))

(defmethod lookup-generic-pos (name pos &key (test 'equal))
  (let ((alist (gethash name *pos-maps*)))
    (when alist
      (let ((tag (car (assoc pos alist :test test))))
        (when tag
          (intern (string-upcase tag) :keyword))))))
