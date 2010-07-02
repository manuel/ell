CFLAGS=-std=c99 -g
LD=gcc
LDFLAGS=-lgc -ldl -rdynamic

ELLC_OBJECTS = ellc.o libellc.o libell.o dict.o list.o

ellc: $(ELLC_OBJECTS)
	$(LD) $(LDFLAGS) $(ELLC_OBJECTS) -o ellc

.PHONY: clean
clean:
	@rm -f *.o ellc
