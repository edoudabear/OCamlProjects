CFLAGS=-Wall -pedantic -std=c99
LDFLAGS=

.PHONY: all run clean mrproper

dlx.o: dlx.c
	gcc $(CFLAGS) -c dlx.c

dlx: dlx.o
	gcc $(LDFLAGS) -o dlx dlx.o

all: dlx

run: all
	./dlx

clean:
	rm -f *.o core

mrproper: clean
	rm -f dlx


