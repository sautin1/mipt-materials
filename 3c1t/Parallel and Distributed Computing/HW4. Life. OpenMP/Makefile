CC      = gcc
LINK    = g++
LIBS    = -lpthread
FLAGS   = -Wall -std=c99 -fopenmp
OBJS    = life4.o commands.o life_grid.o utilities.o

all: life4

life4: $(OBJS)
    $(LINK) $(FLAGS) $(OBJS) -o life4 $(LIBS)

life4.o: life4.c
    $(CC) $(FLAGS) -c life4.c

commands.o: commands.c commands.h
    $(CC) $(FLAGS) -c commands.c

life_grid.o: life_grid.c life_grid.h
    $(CC) $(FLAGS) -c life_grid.c

utilities.o: utilities.c utilities.h
    $(CC) $(FLAGS) -c utilities.c

clean:
    rm -f life4 $(OBJS)
