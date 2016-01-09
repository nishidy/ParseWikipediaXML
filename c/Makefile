PROG= ParseWikipediaXML
GCC= gcc
CFLAGS= -Wall -O2 -pthread
STD= gnu99
LIBS= -lpthread

$(PROG):$(PROG).o
	$(GCC) main.o $^ -o $@ $(LIBS)

$(PROG).o:$(PROG).c
	$(GCC) -c main.c $^ $(CFLAGS) -std=$(STD)

.PHONY: clean
clean:
	rm $(PROG).o main.o

