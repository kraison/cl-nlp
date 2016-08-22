(in-package :nlp)

(let ((number-regex
       (create-scanner
        "^[£\\%\\#±¥\\$\\+]?[½¾0123456789\\-./,:°º\\+]+_?[\\%°£$¥]?$"
        :single-line-mode t))
      (verb-regex (create-scanner
                   "\\w(ar|er|ir|as|amos|áis|an|es|emos|éis|imos|ís|en)$"
                   :single-line-mode t :case-insensitive-mode t))
      (adv-regex (create-scanner "\\w(mente)$"
                                 :single-line-mode t :case-insensitive-mode t))
      (noun-regex
       (create-scanner
        "\\w(stra|stro|ista|ito|ita|illo|illa|ico|ica|ín|ina|ino|ina|iño|iña|uco|uca|eto|eta|ete|uelo|uela)$"
        :single-line-mode t :case-insensitive-mode t))
      (pnoun-regex (create-scanner "^[A-Z]\\w"
                                   :single-line-mode t
                                   :case-insensitive-mode nil)))

  (defun spanish-number-p (text)
    (scan number-regex text))

  (defmethod clean-lexicon ((language spanish))
    "Scrub junk from the lexicon, spcifically various number patterns that are
easy to detect via regex."
    (map-lexicon (lambda (word pos-seq)
                   (declare (ignore pos-seq))
                   (when (or (spanish-number-p word)
                             (hex-number-p word))
                     (remove-from-lexicon-dbm language word)))
                 language))

  (defmethod in-lexicon-p ((language spanish) word pos)
    "Is word / part-of-speech pair in the lexicon?"
    (when (symbolp word) (setq word (symbol-name word)))
    (or (member pos (lookup-pos-dbm language word))
        (member pos (lookup-pos-dbm language (string-downcase word)))
        (and (or (eq pos :Z)
                 (eq pos :ZM)
                 (eq pos :ZP))
             (scan number-regex word))))

  (defmethod lookup-pos ((language spanish) word)
    "Return all possible parts of speech for word"
    (let ((pos-list (or (lookup-pos-dbm language word)
                        (lookup-pos-dbm language (string-downcase word)))))
      (when (scan number-regex word)
        (pushnew :Z pos-list))
      (remove-duplicates
       (remove-if
        'null
        (append
         (or pos-list
             (append
              (and (scan noun-regex word)   (list :NC :W))
              (and (scan *host-regex* word) (list :NP :W))
              (and (scan *ip-regex* word)   (list :NC :W))
              (and (scan pnoun-regex word)  (list :NP :W))
              (and (scan verb-regex word)   (list :VA :VM :VS))
              (and (scan adv-regex word)    (list :RG))))
         (mapcan (lambda (pair)
                   (and (scan (car pair) word) (list (cdr pair))))
                 (user-pos-regex language))))))))
