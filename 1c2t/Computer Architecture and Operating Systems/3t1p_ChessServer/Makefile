#сборка = компиляция + линковка
CC		= gcc
LINK	= g++
LIBS	= -lpthread -lrt
FLAGS	= -std=gnu99 -Wall
DESTDIR	=
TARGET 	= Client \
		  Server

all: Client Server

Server: messages.o server.o chess.o
		$(LINK) messages.o server.o chess.o $(FLAGS) -o Server $(LIBS)

Client: client.o messages.o
		$(LINK) messages.o client.o $(FLAGS) -o Client $(LIBS)

chess.o: chess.c chess.h messages.h
		$(CC) -c $(FLAGS) -o chess.o chess.c

messages.o: messages.c messages.h common.h
		$(CC) -c $(FLAGS) -o messages.o messages.c

server.o: server.c server.h common.h chess.h
		$(CC) -c $(FLAGS) -o server.o server.c

client.o: client.c common.h
		$(CC) -c $(FLAGS) -o client.o client.c

clean:
	rm -f Client Server messages.o server.o client.o
