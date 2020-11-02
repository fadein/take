CSC ?= csc
CSC_OPTIONS ?= -static
INSTALL ?= install
RM      ?= rm
RMDIR   ?= rmdir
SUDO    ?= sudo

BINDIR  ?= /usr/local/bin

take: take.scm
	$(CSC) $(CSC_OPTIONS) $^

test: take
	./take 2 seconds to say hi then take 5 seconds to smile then take 10 seconds to really think about it

install: take
	$(SUDO) $(INSTALL) -d -m 775 $(BINDIR)
	$(SUDO) $(INSTALL) -p -m 755 -t $(BINDIR) $^

uninstall:
	-$(SUDO) $(RM) $(BINDIR)/take

clean:
	-rm -f *.o *.c *.link core take

.PHONY: clean install test
