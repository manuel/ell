CFLAGS=-std=c99 -g -pg
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid -pg

OBJECTS = ellrt.o ellc.o ellcm.o dict.o list.o

all: ell ell-compile

ell: $(OBJECTS) ell.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell.o -o ell

ell-compile: $(OBJECTS) ell-compile.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-compile.o -o ell-compile

.PHONY: clean
clean:
	@rm -f *.o *.fasl ell ell-compile
