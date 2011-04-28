CFLAGS=-std=c99 -g
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid

OBJECTS = ellrt.o ellc.o ellcm.o dict.o list.o
FASLS = lisp-bootstrap-fasls

all: $(OBJECTS) ell-load ell-compile $(FASLS)

lisp-bootstrap-fasls: lisp-bootstrap.lisp $(OBJECTS)
	./ell-compile -c ./lisp-bootstrap.lisp

ell-load: $(OBJECTS) ell-load.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-load.o -o ell-load

ell-compile: $(OBJECTS) ell-compile.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-compile.o -o ell-compile

grammar:
	leg -o grammar.c grammar.leg

debug-grammar:
	make grammar && touch ellrt.c && make ell-load && ./ell

.PHONY: clean
clean:
	@rm -f *.o *.fasl gmon.out ell-load ell-compile
