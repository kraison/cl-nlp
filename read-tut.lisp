(in-package :nlp)

(defun extract-tut-symbols (file)
  "Utility function that lists unique pos tags in a TUT grammar tree file"
  (let ((symbols nil))
    (map-sexp-corpus
     (lambda (tree)
       (map nil
            (lambda (pair)
              (pushnew (second pair) symbols))
            (flatten-phrase-tree tree)))
     file)
    (sort symbols 'string-lessp :key 'symbol-name)))

(defun translate-tut-file (in-file out-file)
  "TUT distributes a Penn-style grammar tree, but uses idiomatic tags;  this
function translates those into tags in Paisa equivalents"
  (with-open-file (out out-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type 'character
                       :external-format :utf-8)
    (map-sexp-corpus
     (lambda (tree)
       (write
        (list
         (nsublis '(("^ADJ~PO"  . :AP)
                    ("^ADJ"     . :A)
                    ("^ADVB"    . :B)
                    ("^ART-IN"  . :RI)
                    ("^ART"     . :RD)
                    ("^CONJ"    . :CC)
                    ("^DATE"    . :N)
                    ("^NOU-PR"  . :SP)
                    ("^NOU"     . :S)
                    ("^NUMR"    . :N)
                    ("^PHRAS"   . :B)
                    ("^PRDT"    . :T)
                    ("^PREP"    . :E)
                    ("^PRO~DE"  . :PD)
                    ("^PRO~ID"  . :PI)
                    ("^PRO~IN"  . :PQ)
                    ("^PRO~LO"  . :PC)
                    ("^PRO~OR"  . :PQ)
                    ("^PRO~PE"  . :PE)
                    ("^PRO~PO"  . :PP)
                    ("^PRO~RE"  . :PR)
                    ("^PRO~RI"  . :PC)
                    ("^PUNCT"   . :FF)
                    ("^SPECIAL" . :X)
                    ("^VAU"     . :VA)
                    ("^VMA"     . :V)
                    ("^VMO"     . :VM))
                  tree
                  :test (lambda (tag pattern)
                          (when (atom tag)
                            (scan pattern (format nil "~A" tag))))))
        :stream out
        :readably t)
       (terpri out))
     in-file)))
