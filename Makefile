SHELL = /bin/sh
INSTALL = install -c

VERSION = 2.24

PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
MANDIR = $(PREFIX)/share/man

SRCFILES = Makefile listadmin.pl listadmin.man

all:
	@echo Nothing needs to be done

install:
	$(INSTALL) listadmin.pl $(bindir)/listadmin
	$(INSTALL) -m 644 listadmin.man $(mandir)/man1/listadmin.1

listadmin.txt: listadmin.man
#	Note the verbatim backspace in the sed command
	env TERM=dumb nroff -man $< | sed 's/.//g' | uniq > $@.tmp
	mv $@.tmp $@

TARFILE = listadmin-$(VERSION).tar.gz
$(TARFILE): $(SRCFILES) listadmin.txt
	@rm -rf listadmin-$(VERSION)
	mkdir listadmin-$(VERSION)
	cp $(SRCFILES) listadmin.txt listadmin-$(VERSION)/
	tar cf - listadmin-$(VERSION) | gzip -9 > $(TARFILE)
	rm -rf listadmin-$(VERSION)

dist: $(TARFILE)

distclean:
	rm -rf $(TARFILE) listadmin.txt listadmin-$(VERSION)

# for my use only
WWW_DOCS = /hom/kjetilho/www_docs/hacks
publish: dist
	cp listadmin.txt $(WWW_DOCS)/listadmin.txt
	cp $(TARFILE) $(WWW_DOCS)/
	perl -pi -e 's/listadmin-\d+\.\d+/listadmin-'$(VERSION)'/g' $(WWW_DOCS)/index.html
