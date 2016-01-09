CC      = gcc
MPICC   = mpicc
LINK    = mpicc
FLAGS   = -Wall -std=c99
OBJS    = life2.o commands.o life_grid.o utilities.o worker.o master.o

all: life2

life2: $(OBJS)
    $(LINK) $(OBJS) -o life2

life2.o: life2.c
    $(MPICC) $(FLAGS) -c life2.c

master.o: master.c master.h
    $(MPICC) $(FLAGS) -c master.c 

worker.o: worker.c worker.h
    $(MPICC) $(FLAGS) -c worker.c

commands.o: commands.c commands.h
    $(MPICC) $(FLAGS) -c commands.c

life_grid.o: life_grid.c life_grid.h
    $(CC) $(FLAGS) -c life_grid.c

utilities.o: utilities.c utilities.h
    $(CC) $(FLAGS) -c utilities.c

clean:
    rm -f life2 $(OBJS)
