CC=gcc
CFLAGS=-std=c99 -Wall
LDFLAGS=-lgc

OBJECTS = ellc.o dict.o list.o

ell: $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

.PHONY: clean
clean:
	@rm -f *.o ell
