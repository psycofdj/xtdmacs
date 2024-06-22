SOURCES = $(wildcard *.el bin/* vendor/* | grep -v autoloader)
VERSION = $(shell cat xtdmacs-pkg.el | grep define-package | awk '{print $$3}' | sed 's/"//g')
DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))

all: install

dist: xtdmacs-$(VERSION).tar

xtdmacs-$(VERSION).tar: $(SOURCES)
	@mkdir xtdmacs-$(VERSION)
	@cp --parents -dR $(SOURCES) xtdmacs-$(VERSION)/
	@tar cvf xtdmacs-$(VERSION).tar xtdmacs-$(VERSION)
	@rm -rf xtdmacs-$(VERSION)

install: xtdmacs-$(VERSION).tar
	@rm -rf ~/.emacs.d/elpa/xtdmacs*/
	@mkdir -p ~/.emacs.d/elpa/gnupg
	@gpg --homedir ~/.emacs.d/elpa/gnupg --keyserver hkp://keyserver.ubuntu.com  --recv-keys 645357D2883A0966
	@emacs --batch --eval "(defconst pkg-to-install \"$(DIR)/xtdmacs-$(VERSION).tar\")" --eval "(defconst pkg-refresh t)" -l vendor/emacs-pkg-install.el

install-quick: xtdmacs-$(VERSION).tar
	@rm -rf ~/.emacs.d/elpa/xtdmacs*/
	@emacs --batch --eval "(defconst pkg-to-install \"$(DIR)/xtdmacs-$(VERSION).tar\")" --eval "(defconst pkg-refresh nil)" -l vendor/emacs-pkg-install.el
