listadmin.txt: listadmin.1
	env TERM=dumb nroff -c -man $< | sed 's/.//g' | uniq > $@.tmp
	mv $@.tmp $@
	@echo manual cleanup is needed
