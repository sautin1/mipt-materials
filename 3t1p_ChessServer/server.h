#include <pthread.h>
#include "messages.h"
#include "common.h"
//#define SOCKNAME "mysocket"
#define MAX_USERS 5
#define MAX_LISTENED 10
#define PAUSE_LENGTH 10000
typedef UserData User;

void readMessage(MessageType* m, int incomeSd, size_t* user_id);
void* establishConnection(void* arg);
void* manageConnections(void* arg);
