CSC ?= csc
CSC_OPTIONS ?= -static
INSTALL ?= install
RM      ?= rm
RMDIR   ?= rmdir
SUDO    ?= sudo

BINDIR  ?= /usr/local/bin

PROG    = take


$(PROG): $(PROG).scm
	$(CSC) $(CSC_OPTIONS) $^

test: $(PROG)
	./take 2 seconds to say hi then take 5 seconds to smile then take 10 seconds to really think about it

install: $(PROG)
	$(SUDO) $(INSTALL) -d -m 775 $(BINDIR)
	$(SUDO) $(INSTALL) -p -m 755 -t $(BINDIR) $^

uninstall:
	-$(SUDO) $(RM) $(BINDIR)/$(PROG)

clean:
	-rm -f *.o *.c *.link *core $(PROG)

setup:
	chicken-install -sudo -from-list eggs.list

define HELP =
Makefile for "$(PROG)"

Available targets:
    test		Build & test the executable "$(PROG)" [DEFAULT TARGET]
    $(PROG)		Build the executable
    install		Install the executable
    uninstall		Uninstall the executable
    clean		Clean up the build directory
    setup		Install any required eggs

endef

help: ; $(info $(HELP)) @:

.PHONY: test install uninstall clean setup help
