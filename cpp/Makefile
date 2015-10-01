PROG= ParseWikipediaXML
GCC= g++
CFLAGS= -Wall -O2
STD= c++14
LIBS= -lboost_program_options -lboost_thread -lpthread -lboost_system -lboost_regex -lmecab

$(PROG):$(PROG).o
	$(GCC) $^ -o $@ $(LIBS)

$(PROG).o:$(PROG).cpp
	$(GCC) -c $^ $(CFLAGS) -std=$(STD)

.PHONY: clean
clean:
	rm $(PROG).o

