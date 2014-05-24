#ifndef MESSAGES_H
#define MESSAGES_H

#include "common.h"
#define MAX_NAME_LENGTH 50
#define MAX_LEVEL 10

void throwError(char* error);

//common type for messages
typedef struct {
	enum Type {login, logout, turn, disposition, result, userlist} type;
	size_t size;
	void* data;
} MessageType;

typedef struct {
    char name[MAX_NAME_LENGTH]; //static array to use sizeof
    int ownLevel;
    int desiredLevel;
} UserData;

MessageType composeMessage(enum Type type, size_t size, void* data);
int getMessage(int sd, MessageType* m);
int sendMessage(int sd, MessageType* m);

#endif //MESSAGES_H
