CC      = gcc
LINK    = g++
LIBS    = -lpthread
FLAGS   = -Wall -std=c99
OBJS    = life1.o commands.o life_grid.o utilities.o

all: life1

life1: $(OBJS)
    $(LINK) $(FLAGS) $(OBJS) -o life1 $(LIBS)

life1.o: life1.c
    $(CC) $(FLAGS) -c life1.c

commands.o: commands.c commands.h
    $(CC) $(FLAGS) -c commands.c

life_grid.o: life_grid.c life_grid.h
    $(CC) $(FLAGS) -c life_grid.c

utilities.o: utilities.c utilities.h
    $(CC) $(FLAGS) -c utilities.c

clean:
    rm -f life1 $(OBJS)
