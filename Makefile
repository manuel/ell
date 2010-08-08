CFLAGS=-std=c99 -g -pg
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid -pg

OBJECTS = ellrt.o ellc.o ellcm.o dict.o list.o

all: $(OBJECTS) ell-repl ell-compile lisp

lisp: $(OBJECTS)
	./ell-compile -c ./lisp-0.lisp -c ./lisp-conditions.lisp
	./ell-compile -x ./lisp-0.lisp.syntax.fasl -x ./lisp-conditions.lisp.syntax.fasl -c ./lisp-library-helpers.lisp
	./ell-compile -x ./lisp-0.lisp.syntax.fasl -x ./lisp-conditions.lisp.syntax.fasl -x ./lisp-library-helpers.lisp.load.fasl -c lisp-library.lisp

ell-repl: $(OBJECTS) ell-repl.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-repl.o -o ell-repl

ell-compile: $(OBJECTS) ell-compile.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-compile.o -o ell-compile

grammar:
	leg -o grammar.c grammar.leg

.PHONY: clean
clean:
	@rm -f *.o *.fasl gmon.out ell-repl ell-compile
