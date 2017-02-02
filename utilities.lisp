(in-package :nlp)

(defun /-safe (x y)
  "Safe division"
  (handler-case
      (/ x y)
    (division-by-zero (c)
      (declare (ignore c))
      0)
    (error (c)
      (declare (ignore c))
      0)))

(defun /-double (x y)
  "Safe double precision division"
  (cond ((zerop x) 0.0d0)
        ((zerop y) 0.0d0)
        (t
         (handler-case
             (/ x y)
           (division-by-zero (c)
             (declare (ignore c))
             0.0d0)
           (error (c)
             (declare (ignore c))
             0.0d0)))))

(defun square (x)
  "Square a number"
  (* x x))

(defun sum (list)
  "Sum a list of numbers"
  (apply #'+ list))

(defun vector-last (vector)
  (elt vector (1- (array-dimension vector 0))))

(defmacro while (pred &body body)
  `(loop (unless ,pred (return nil)) ,@body))

(defmethod counting-intersection ((v1 array) (v2 array) &key (test 'eql))
  (let ((i (make-array '(0) :adjustable t :fill-pointer t)) (count 0))
    (loop for n across v1 do
         (when (find n v2 :test test)
           (incf count)
           (vector-push-extend n i)))
    (values i count)))

(defmethod counting-intersection ((v1 list) (v2 list) &key (test 'eql))
  (let ((i ()) (count 0))
    (loop for n in v1 do
         (when (find n v2 :test test)
           (incf count)
           (push n i)))
    (values i count)))

(defun union-all (list-of-lists &key (test 'eql) (key 'identity))
  "Find the union of an arbitrary number of sets"
  (labels
      ((my-union (ll)
         (let ((length (list-length ll)))
           (cond ((= length 0) nil)
                 ((= length 1) (first ll))
                 ((= length 2)
                  (union (first ll) (second ll) :test test :key key))
                 ((> length 2)
                  (my-union
                   (cons
                    (union (first ll) (second ll) :test test :key key)
                    (cddr ll))))))))
    (my-union (sort list-of-lists #'> :key #'length))))

;;; Thanks, Mr. Norvig for this queueing code
(defun print-queue (q stream depth)
  (declare (ignore depth))
  (format stream "<QUEUE: ~a>" (queue-elements q)))

(defstruct (queue
             (:print-function print-queue))
  (key #'identity)
  (last nil)
  (elements nil))

(defun make-empty-queue () (make-queue))

(defun empty-queue-p (q)
  (= (length (queue-elements q)) 0))

(defun queue-front (q)
  (elt (queue-elements q) 0))

(defun dequeue (q)
  (when (listp (queue-elements q))
    (pop (queue-elements q))))

(defun enqueue (q items)
  (cond ((null items) nil)
        ((or (null (queue-last q)) (null (queue-elements q)))
         (setf (queue-last q) (last items)
               (queue-elements q) (nconc (queue-elements q) items)))
        (t (setf (cdr (queue-last q)) items
                 (queue-last q) (last items)))))

(defun queue-length (q)
  (length (queue-elements q)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?"
  (if (atom tree)
      (if (eql item tree) tree)
      (or (find-anywhere item (first tree))
          (find-anywhere item (rest tree)))))

(defun last1 (list)
  "Return the last item in a list"
  (car (last list)))

(defun second-to-last (list)
  "Return the second to last item in a list"
  (let ((l (length list)))
    (when (> l 1)
      (car (subseq list (- l 2) (1- l))))))

(defun flatten (lis)
  "Flatten a tree into a list"
  (cond ((atom lis) lis)
        ((listp (car lis))
         (append (flatten (car lis)) (flatten (cdr lis))))
        (t (append (list (car lis)) (flatten (cdr lis))))))

(defun init-or-increment (hash thing)
  "Increment or add a value for this hash key"
  (incf (gethash thing hash 0)))

(defun join (seq &optional (delimiter " "))
  "Join a seq into a space-delimited string"
  (with-output-to-string (out)
    (dotimes (i (length seq))
      (format out "~A" (elt seq i))
      (unless (= i (1- (length seq)))
        (format out "~A" delimiter)))))

(defun quit (&optional code)
  ;; This group from "clocc-port/ext.lisp"
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code)
  #+cormanlisp (win32:exitprocess code)
  #+gcl (lisp:bye code)                 ; XXX Or is it LISP::QUIT?
  #+lispworks (lw:quit :status code)
  #+lucid (lcl:quit code)
  #+sbcl (sb-ext:exit :code code)
  ;; This group from Maxima
  #+kcl (lisp::bye)                     ; XXX Does this take an arg?
  #+scl (ext:quit code)                 ; XXX Pretty sure this *does*.
  #+(or openmcl mcl) (ccl::quit)
  #+abcl (cl-user::quit)
  #+ecl (si:quit)
  ;; This group from <hebi...@math.uni.wroc.pl>
  #+poplog (poplog::bye)                ; XXX Does this take an arg?
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
        kcl scl openmcl mcl abcl ecl)
  (error 'not-implemented :proc (list 'quit code)))
