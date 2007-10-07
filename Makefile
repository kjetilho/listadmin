SHELL = /bin/sh
# a BSD or GNU style install is required, e.g., /usr/ucb/install on Solaris
INSTALL = install

VERSION = 2.40

PREFIX = /usr/local
prefix = $(PREFIX)
bindir = $(prefix)/bin
mandir = $(prefix)/share/man

SRCFILES = Makefile listadmin.pl listadmin.man

all:
	@echo Nothing needs to be done

install:
	$(INSTALL) -d $(DESTDIR)$(bindir) $(DESTDIR)$(mandir)/man1
	$(INSTALL) -m 755 listadmin.pl $(DESTDIR)$(bindir)/listadmin
	$(INSTALL) -m 644 listadmin.man $(DESTDIR)$(mandir)/man1/listadmin.1

listadmin.txt: listadmin.man
#	Note the verbatim backspace in the sed command
	env TERM=dumb nroff -man $< | sed -e '/^XXX/d' -e 's/.//g' | uniq > $@.tmp
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
	cp -p listadmin.txt $(WWW_DOCS)/listadmin.txt
	cp -p $(TARFILE) $(WWW_DOCS)/
	cp -p listadmin.pl $(WWW_DOCS)/listadmin
	cp -p listadmin.man $(WWW_DOCS)/listadmin.man
	perl -pi -e 's/listadmin(.)\d+\.\d+/listadmin$${1}'$(VERSION)'/g' $(WWW_DOCS)/index.html
