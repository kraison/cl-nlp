# NLP Service Makefile

CC=gcc
LISP=sbcl
DEPS_STATUS=deps.status

all: deps train-all

data:
	aws s3 cp s3://answerdash.resources/cl-nlp/data/ ./data/ --recursive

deps:
	sudo apt-get install -y -f --force-yes wordnet graphviz ed libstemmer-dev libstemmer-tools libstemmer0d libkyotocabinet-dev libkyotocabinet16
	sudo rm -f /usr/lib/libwordnet.so
	sudo ln -s /usr/lib/libwordnet-3.0.so /usr/lib/libwordnet.so
	echo "ok" > $(DEPS_STATUS)

train-all: clean deps train-english train-german train-spanish train-italian train-portuguese train-french
	tar czf languages.tar.gz \
	  english.dat english-*.kch \
	  german.dat german-*.kch \
	  spanish.dat spanish-*.kch \
	  french.dat french-*.kch \
	  portuguese.dat portuguese-*.kch \
	  italian.dat italian-*.kch

train-english: deps
	$(LISP) --dynamic-space-size 16384 --non-interactive --load train-english.lisp

train-german: deps
	$(LISP) --dynamic-space-size 16384 --non-interactive --load train-german.lisp

train-spanish: deps
	$(LISP) --dynamic-space-size 16384 --non-interactive --load train-spanish.lisp

train-italian: deps
	$(LISP) --dynamic-space-size 16384 --non-interactive --load train-italian.lisp

train-portuguese: deps
	$(LISP) --dynamic-space-size 16384 --non-interactive --load train-portuguese.lisp

train-french: deps
	$(LISP) --dynamic-space-size 16384 --non-interactive --load train-french.lisp

test:
	$(LISP) --dynamic-space-size 16384 --non-interactive --load test.lisp

push: train-all
	aws s3 cp languages.tar.gz s3://answerdash.resources/cl-nlp/languages.tar.gz

clean:
	$(RM) $(DEPS_STATUS)
	$(RM) english.dat english*.kch
	$(RM) german.dat german*.kch
	$(RM) portuguese.dat portuguese*.kch
	$(RM) spanish.dat spanish*.kch
	$(RM) italian.dat italian*.kch
	$(RM) french.dat french*.kch
	$(RM) languages.tar.gz

# DO NOT DELETE THIS LINE -- make depend needs it
