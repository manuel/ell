CFLAGS=-std=c99 -g -pg
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid -pg

OBJECTS = ellrt.o ellc.o ellcm.o dict.o list.o

all: $(OBJECTS) ell-repl ell-compile boot conditions

boot: $(OBJECTS) boot.lisp
	./ell-compile -c boot.lisp

conditions: $(OBJECTS) conditions.lisp boot.lisp.syntax.fasl
	./ell-compile -x ./boot.lisp.syntax.fasl -c conditions.lisp

grammar:
	leg -o grammar.c grammar.leg

ell-repl: $(OBJECTS) ell-repl.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-repl.o -o ell-repl

ell-compile: $(OBJECTS) ell-compile.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-compile.o -o ell-compile

.PHONY: clean
clean:
	@rm -f *.o *.fasl gmon.out ell-repl ell-compile
