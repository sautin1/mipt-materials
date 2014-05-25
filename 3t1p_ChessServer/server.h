#include <pthread.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "messages.h"
#include "common.h"

#define MAX_USERS 5
#define MAX_LISTENED 10
#define PAUSE_LENGTH 10000

typedef UserData User;

typedef struct {
    User list[MAX_USERS];
    size_t q;
} UserList;

#define SHM_NAME "mysharedmemory"
#define SHM_SIZE sizeof(UserList)

//Children's processes
void readMessage(MessageType* m, int incomeSd, size_t* user_id, void* arg);
void* establishConnection(int incomeSd, void* arg);
//Parent's process
void* manageConnections(void* arg);
