EMACS ?= emacs

INIT_PACKAGES="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (dolist (pkg '(f let-alist seq s dash reformatter package-lint)) \
    (unless (package-installed-p pkg) \
      (unless (assoc pkg package-archive-contents) \
        (package-refresh-contents)) \
      (package-install pkg))) \
  )"

all: compile package-lint clean-elc

package-lint:
	${EMACS} -Q --eval ${INIT_PACKAGES} -batch -f package-lint-batch-and-exit elm-mode.el

compile: clean-elc
	${EMACS} -Q --eval ${INIT_PACKAGES} -L . -batch -f batch-byte-compile *.el

clean-elc:
	rm -f f.elc

.PHONY:	all compile clean-elc package-lint
