CFLAGS=-std=c99 -g
LD=gcc
LDFLAGS=-rdynamic -lgc -ldl -lreadline -luuid

OBJECTS = ell.o ellrt.o ellc.o dict.o list.o

ell: $(OBJECTS)
	$(LD) $(LDFLAGS) $(OBJECTS) -o ell

.PHONY: clean
clean:
	@rm -f *.o ell ell
