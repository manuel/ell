CFLAGS=-std=c99 -g
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline

ELL_OBJECTS = ell.o libellc.o libell.o dict.o list.o
ELLC_OBJECTS = ellc.o libellc.o libell.o dict.o list.o

ell: $(ELL_OBJECTS)
	$(LD) $(LDFLAGS) $(ELL_OBJECTS) -o ell

ellc: $(ELLC_OBJECTS)
	$(LD) $(LDFLAGS) $(ELLC_OBJECTS) -o ellc

.PHONY: clean
clean:
	@rm -f *.o ell ellc
