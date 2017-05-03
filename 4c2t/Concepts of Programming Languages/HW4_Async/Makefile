OUTFILE=main

all: clean compile

compile:
	g++ -pthread -std=c++11 -Wall -o $(OUTFILE) *.cpp -lgtest

clean:
	rm -f $(OUTFILE)
