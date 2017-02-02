(push #P"./lib/cffi-wordnet/" asdf:*central-registry*)
(push #P"./lib/cl-daemonize/" asdf:*central-registry*)
(push #P"./lib/graph-utils/" asdf:*central-registry*)
(push #P"./lib/porter-stemmer/" asdf:*central-registry*)
(push #P"./" asdf:*central-registry*)

(ql:quickload :nlp)
(quit 0)
