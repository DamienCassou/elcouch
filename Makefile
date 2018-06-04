SRCS = elcouch.el

LOAD_PATH = -L . -L ../json-mode -L ../json-reformat -L ../json-snatcher -L ../package-lint

EMACSBIN ?= emacs
BATCH     = $(EMACSBIN) -Q --batch $(LOAD_PATH) \
		--eval "(setq load-prefer-newer t)" \
		--eval "(require 'package)" \
		--eval "(add-to-list 'package-archives '(\"melpa-stable\" . \"http://stable.melpa.org/packages/\"))" \
		--eval "(setq enable-dir-local-variables nil)" \
		--funcall package-initialize

CURL=curl -fsSkL --retry 9 --retry-delay 9
GITHUB=https://raw.githubusercontent.com
GITLAB=https://gitlab.petton.fr

.PHONY: all ci-dependencies check lint

all: check

ci-dependencies:
	# Install dependencies in ~/.emacs.d/elpa
	$(BATCH) \
	--funcall package-refresh-contents \
	--eval "(package-install 'json-mode)" \
	--eval "(package-install 'request)"


	# Install package-lint from master as latest release is buggy
	# https://github.com/purcell/package-lint/pull/106
	$(CURL) -O ${GITHUB}/purcell/package-lint/master/package-lint.el

	# Install libelcouch separately as the version on melpa takes
	# several hours to build and is then regularly out of date:
	$(CURL) -O ${GITLAB}/elcouch/libelcouch/raw/master/libelcouch.el

check: lint

lint :
	# Byte compile all and stop on any warning or error
	$(BATCH) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile ${SRCS}

	# Run package-lint to check for packaging mistakes
	$(BATCH) \
	--eval "(require 'package-lint)" \
	--eval "(package--add-to-archive-contents '(libelcouch . [(0 8 0) ((emacs (25 1))) \"Some documentation\" single ()]) \"melpa-stable\")" \
	-f package-lint-batch-and-exit ${SRCS}

	# Run checkdoc to check Emacs Lisp conventions
	$(BATCH) --eval "(mapcar #'checkdoc-file '($(patsubst %, \"%\", ${SRCS})))"
