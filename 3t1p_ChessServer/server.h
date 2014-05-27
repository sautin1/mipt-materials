#ifndef SERVER_H
#define SERVER_H


#include <pthread.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "messages.h"
#include "common.h"
#include "chess.h"

#define MAX_USERS 5
#define MAX_GAMES MAX_USERS/2
#define MAX_TOTAL_USERS 1000
#define MAX_LISTENED 10
#define PAUSE_LENGTH 10000

typedef UserData User;

typedef struct {
    User list[MAX_USERS];
    short int color[MAX_USERS]; //0 - is not playing, 1 - is white, 2 - is black
    size_t q;
    TGame game[MAX_USERS];
    int usergame[MAX_USERS];
} UserGameList;

#define SHM_NAME "mysharedmemory"
#define SHM_SIZE sizeof(UserGameList)

//Children's processes
size_t createOpponentList(void* arg1, void* arg2, size_t user_id);
void readMessage(MessageType* m, int incomeSd, size_t* user_id, void* arg);
void* establishConnection(int incomeSd, void* arg);
//Parent's process
void* manageConnections(void* arg);

#endif // SERVER_H
