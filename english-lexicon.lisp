(in-package :nlp)

(let ((verb-regex (create-scanner
                   "\\w(ing|ate|ify|ize|ise|ed)$"
                   :single-line-mode t :case-insensitive-mode t))
      (adj-regex (create-scanner
                  "\\w(able|ible|al|ial|ic|y|ing|ed|ful|ish|ive|ous|ious)$"
                  :single-line-mode t :case-insensitive-mode t))
      (adv-regex (create-scanner
                  "\\w(ly|ally|ily)$"
                  :single-line-mode t :case-insensitive-mode t))
      (noun-regex
       (create-scanner
        "\\w(er|or|ance|ence|ant|ent|ee|ess|ian|ism|ics|ist|ity|ment|ness|ship|tion|ation|ure|man|woman|eur|ing|hood)$"
        :single-line-mode t :case-insensitive-mode t))
      (pnoun-regex (create-scanner
                    "^[A-Z]\\w" :single-line-mode t :case-insensitive-mode nil)))

  (defmethod in-lexicon-p ((language english) word pos)
    "Is word as pos in the lexicon?"
    (when (symbolp word) (setq word (symbol-name word)))
    (or (member pos (lookup-pos-dbm language word))
        (member pos (lookup-pos-dbm language (string-downcase word)))
        (and (eq pos :CD) (scan *number-regex* word))))

  (defmethod lookup-pos ((language english) word)
    "Return all possible parts of speech for word"
    (let ((pos-list (or (lookup-pos-dbm language word)
                        (lookup-pos-dbm language (string-downcase word)))))
      (when (or (scan *number-regex* word)
                (hex-number-p word))
        (pushnew :CD pos-list))
      (remove-duplicates
       (remove-if
        'null
        (append
         (or pos-list
             (append
              (and (scan noun-regex word)  (list :NN))
              (and (scan *host-regex* word)  (list :NNP))
              (and (scan *ip-regex* word)    (list :NN))
              (and (scan pnoun-regex word) (list :NNP))
              (and (scan verb-regex word)  (list :VB))
              (and (scan adv-regex word)   (list :RB))
              (and (scan adj-regex word)   (list :JJ))))
         (mapcan (lambda (pair)
                   (and (scan (car pair) word) (list (cdr pair))))
                 (user-pos-regex language))))))))
