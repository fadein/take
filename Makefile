CSC ?= csc
CSC_OPTIONS ?= 
INSTALL ?= install
RM      ?= rm
RMDIR   ?= rmdir

BINDIR  ?= /usr/local/bin

test: take
	./take 20 seconds to say hi

take: take.scm
	$(CSC) $(CSC_OPTIONS) $^

install: take
	$(INSTALL) -d -m 775 $(BINDIR)
	$(INSTALL) -p -m 755 -t $(BINDIR) $^

uninstall:
	-$(RM) $(BINDIR)/take

clean:
	-rm -f *.o *.c *.link core take

.PHONY: clean install test
