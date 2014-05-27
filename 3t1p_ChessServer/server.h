#include <pthread.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "messages.h"
#include "common.h"

#define MAX_USERS 5
#define MAX_GAMES MAX_USERS/2
#define MAX_TOTAL_USERS 1000
#define MAX_LISTENED 10
#define PAUSE_LENGTH 10000

typedef UserData User;

typedef struct {
    User list[MAX_USERS];
    short int busy[MAX_USERS];
    size_t q;
} UserList;

#define SHM_NAME "mysharedmemory"
#define SHM_SIZE sizeof(UserList)

//Children's processes
size_t createOpponentList(void* arg1, void* arg2, size_t user_id);
void readMessage(MessageType* m, int incomeSd, size_t* user_id, size_t* opponent_id, void* arg1, void* arg2);
void* establishConnection(int incomeSd, void* arg);
//Parent's process
void* manageConnections(void* arg);
