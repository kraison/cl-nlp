(in-package #:cl-user)

(defpackage #:nlp
  (:use #:cl #:kyoto-cabinet #:cl-ppcre #:parse-number #:dso-lex #:yacc
        #:graph-utils #:cffi-wordnet)
  (:export #:*language*
           #:*languages*
           #:language
           #:language-p
           #:freeze-nlp
           #:thaw-nlp
           #:lookup-language
           #:close-language
           #:make-language
           #:make-english-db
           #:make-spanish-db
           #:make-portuguese-db
           #:name
           #:default-encoding

           ;; Language classes we support
           #:english
           #:spanish
           #:portuguese
           #:italian
           #:german
           #:french

           #:edit-distance
           #:longest-common-subseq
           #:split-sentences
           #:tokenize
           #:train-tagger
           #:tag-sentence
           #:tag
           #:tag-as-text
           #:noun-p
           #:verb-p
           #:adjective-p
           #:punctuation-p

           #:earley-parse
           #:chart-parse
           #:cyk-parse
           #:pcp-parse
           #:p-chart-parse
           #:extract-phrases
           #:all-phrases
           #:train-phrase-extractor
           #:singularize
           #:extract-ngrams
           #:extract-skip-bigrams
           #:extract-skip-trigrams

           #:in-lexicon-p
           #:add-to-lexicon
           #:lookup-pos
           #:possible-tags
           #:pos-similarity
           #:tree-similarity
           #:pos-edit-distance

           #:spell-check
           #:correct-spelling

           #:join
           #:stem
           #:add-stop-word
           #:remove-stop-word
           #:stop-word-p
           #:remove-stop-words
           #:wildcard-stop-words

           #:synset
           #:singularize
           #:verb-base-form
           #:synonyms
           #:synonym-p
           #:hypernyms
           #:meronyms
           #:instances
           #:holonyms
           #:hyponyms
           #:semantic-neighborhood
           #:semantic-parents
           #:semantic-children
           #:glosses
           #:wordnet-pos
           #:synset-word-list
           #:synset-type

           #:semantic-similarity
           #:semantic-distance
           #:negative-squared-semantic-distance
           #:clear-semantic-caches

           #:flatten
           #:total-word-count
           #:word-occurrence

           #:+punctuation-tag+
           #:+foreign-tag+
           #:+adjective-tag+
           #:+conjunction-tag+
           #:+determiner-tag+
           #:+noun-tag+
           #:+pronoun-tag+
           #:+adverb-tag+
           #:+adposition-tag+
           #:+verb-tag+
           #:+number-tag+
           #:+particle-tag+
           ))
