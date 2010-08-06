CFLAGS=-std=c99 -g
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid

OBJECTS = ellrt.o ellc.o ellcm.o dict.o list.o

fasls: $(OBJECTS) boot.lisp all
	./ell-compile -c boot.lisp

all: ell-repl ell-compile

ell-repl: $(OBJECTS) ell-repl.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-repl.o -o ell-repl

ell-compile: $(OBJECTS) ell-compile.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-compile.o -o ell-compile

.PHONY: clean
clean:
	@rm -f *.o *.fasl ell-repl ell-compile
