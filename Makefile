CFLAGS=-std=c99 -Wall -g
LD=gcc
LDFLAGS=-lgc

OBJECTS = ellc.o dict.o list.o

ellc: $(OBJECTS)
	$(LD) $(LDFLAGS) $(OBJECTS) -o ellc

.PHONY: clean
clean:
	@rm -f *.o ellc
