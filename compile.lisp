(push #P"../cffi-wordnet/" asdf:*central-registry*)
(push #P"../cl-daemonize/" asdf:*central-registry*)
(push #P"../graph-utils/" asdf:*central-registry*)
(push #P"../porter-stemmer/" asdf:*central-registry*)
(push #P"./" asdf:*central-registry*)

(ql:quickload :nlp)
(sb-ext:quit :unix-status 0)
