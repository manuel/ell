CFLAGS=-std=c99 -g
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid

OBJECTS = ellrt.o ellc.o ellcm.o dict.o list.o

ell: $(OBJECTS) ell.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell.o -o ell

ell-compile: $(OBJECTS) ell-compile.o
	$(LD) $(LDFLAGS) $(OBJECTS) ell-compile.o -o ell-compile

.PHONY: clean
clean:
	@rm -f *.o ell ell-compile
