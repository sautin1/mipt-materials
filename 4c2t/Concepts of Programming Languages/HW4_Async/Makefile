OUTFILE=main

all: clean compile

compile:
	g++ -pthread -std=gnu++11 -Wall -o $(OUTFILE) *.cpp -lgtest

clean:
	rm $(OUTFILE)
