## Makefile

all:

REPO_USER    := conao3
PACKAGE_NAME := oj
REPO_NAME    := oj.el

EMACS        ?= emacs
ELS          := $(shell cask files)

GIT_HOOKS    := pre-commit

##################################################

.PHONY: all git-hook help build test clean

all: git-hook help

git-hook: $(GIT_HOOKS:%=.git/hooks/%)

.git/hooks/%: git-hooks/%
	cp -a $< $@

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Install git-hook to your local .git folder)
	$(info   - make build    # Compile Elisp files)
	$(info   - make test     # Compile Elisp files and test $(PACKAGE_NAME))
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files)
	$(info )
	$(info This Makefile required `cask`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

%.elc: %.el .cask
	cask exec $(EMACS) -Q --batch -f batch-byte-compile $<

.cask: Cask
	cask install
	touch $@

##############################

build: $(ELS:%.el=%.elc)

test: build
	cask exec $(EMACS) -Q --batch -L . -l $(PACKAGE_NAME)-tests.el -f cort-test-run

clean:
	rm -rf $(ELS:%.el=%.elc) .cask
