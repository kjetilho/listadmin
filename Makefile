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

dist: listadmin.txt
	@rm -rf listadmin-$(VERSION)
	mkdir -p listadmin-$(VERSION)
	cp $(SRCFILES) listadmin.txt listadmin-$(VERSION)/
	tar cf - listadmin-$(VERSION) | gzip -9 > listadmin-$(VERSION).tar.gz
	rm -rf listadmin-$(VERSION)
