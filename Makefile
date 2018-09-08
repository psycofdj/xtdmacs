SOURCES = $(wildcard *.el bin/* vendor/* | grep -v autoloader)
VERSION = $(shell cat xtdmacs-pkg.el | grep define-package | awk '{print $$3}' | sed 's/"//g')

all: install

dist: xtdmacs-$(VERSION).tar

xtdmacs-$(VERSION).tar: $(SOURCES)
	@mkdir xtdmacs-$(VERSION)
	@cp --parents -dR $(SOURCES) xtdmacs-$(VERSION)/
	@tar cvf xtdmacs-$(VERSION).tar xtdmacs-$(VERSION)
	@rm -rf xtdmacs-$(VERSION)

install: xtdmacs-$(VERSION).tar
	@rm -rf ~/.emacs.d/elpa/xtdmacs*/
	@emacs --batch --eval "(defconst pkg-to-install \"$(PWD)/xtdmacs-$(VERSION).tar\")" -l vendor/emacs-pkg-install.el
	@rm -rf ~/.emacs.d/elpa/go-mode*/*.elc ~/.emacs.d/elpa/go-eldoc*/*.elc
	@patch --forward ~/.emacs.d/elpa/go-mode*/go-mode.el < patches/go-mode.patch || true
	@cp vendor/go-eldoc-pkg.el ~/.emacs.d/elpa/go-eldoc-*/go-eldoc-pkg.el
