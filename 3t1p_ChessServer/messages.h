#ifndef MESSAGES_H
#define MESSAGES_H

#include "common.h"
#define MAX_NAME_LENGTH 50
#define MAX_LEVEL 10

void throwError(char* error);

//общий тип для сообщений
typedef struct {
	enum Type {login, logout, turn, disposition, result, userlist} type;
	size_t size;
	void* data;
} MessageType;

typedef struct {
	char name[MAX_NAME_LENGTH]; //статический массив, чтобы можно было использовать sizeof
	int ownLevel; 		//свой уровень
	int desiredLevel;	//желаемый уровень соперника
} UserData;

//создать сообщение
MessageType composeMessage(enum Type type, size_t size, void* data);
//получить сообщение
int getMessage(int sd, MessageType* m);
//отправить сообщение
int sendMessage(int sd, MessageType* m);

#endif //MESSAGES_H
