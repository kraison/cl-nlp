(in-package :nlp)

(let ((number-regex
       (create-scanner
        "^[£\\%\\#±¥\\$\\+]?[½¾0123456789\\-./,:°º\\+]+_?[\\%°£$¥]?$"
        :single-line-mode t))
      (infinitive-regex (create-scanner
                         "\\w(er|ir|re)$"
                         :single-line-mode t :case-insensitive-mode t))
      (verb-regex (create-scanner
                   "\\w(e|es|ons|ez|ent|is|it|issons|issez|issent)$"
                   :single-line-mode t :case-insensitive-mode t))
      (adv-regex (create-scanner "\\w(ment)$"
                                 :single-line-mode t :case-insensitive-mode t))
      (noun-regex
       (create-scanner
        "\\w()$"
        :single-line-mode t :case-insensitive-mode t))
      (pnoun-regex (create-scanner "^[A-Z]\\w"
                                   :single-line-mode t
                                   :case-insensitive-mode nil)))

  (defun french-number-p (text)
    (scan number-regex text))

  (defmethod in-lexicon-p ((language french) word pos)
    "Is word / part-of-speech pair in the lexicon?"
    (when (symbolp word) (setq word (symbol-name word)))
    (or (member pos (lookup-pos-dbm language word))
        (member pos (lookup-pos-dbm language (string-downcase word)))
        (and (or (eq pos :NC)
                 (scan number-regex word)))))

  (defmethod lookup-pos ((language french) word)
    "Return all possible parts of speech for word"
    (let ((pos-list (or (lookup-pos-dbm language word)
                        (lookup-pos-dbm language (string-downcase word)))))
      (when (scan number-regex word)
        (pushnew :NC pos-list))
      (remove-duplicates
       (remove-if
        'null
        (append
         (or pos-list
             (append
              (and (scan noun-regex word)       (list :NC :NPP))
              (and (scan *host-regex* word)     (list :NPP :NC))
              (and (scan *ip-regex* word)       (list :NC :NPP))
              (and (scan pnoun-regex word)      (list :NPP :NC))
              (and (scan verb-regex word)       (list :V))
              (and (scan infinitive-regex word) (list :VINF))
              (and (scan adv-regex word)        (list :ADV))))
         (mapcan (lambda (pair)
                   (and (scan (car pair) word) (list (cdr pair))))
                 (user-pos-regex language))))))))
