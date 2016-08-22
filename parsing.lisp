(ql:quickload :nlp)
(ql:quickload "trivial-timeout")

(in-package :nlp)

(init-nlp "nlp.dat")

(defun tomuro-cfg (&optional (corpus "data/all-parsed.txt"))
  (load-cfg "data/grammar.txt" *pos-db*)
  (with-open-file (log "tomuro.log"
                       :direction :output
                       :if-exists :supersede)
    (map-sexp-corpus
     (lambda (tree)
       (let ((words (syntax-leaves tree)))
         (format t "~A~%" words)
         (format log "~A~%" words)
         (multiple-value-bind (parse tags)
             (handler-case
                 (trivial-timeout:with-timeout (10)
                   (chart-parse (butlast words)))
               (error (c)
                 (declare (ignore c))
                 (sb-ext:gc :full t)
                 (values nil (possible-tags words))))
           (format t "~A~%~A~%~%" tags parse)
           (format log "~A~%~A~%~%" tags parse))))
     corpus)))

(tomuro-cfg)
(sb-ext:quit)
