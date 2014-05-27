#ifndef MESSAGES_H
#define MESSAGES_H

#include "common.h"
#define MAX_NAME_LENGTH 50
#define MAX_LEVEL 10

void throwError(char* error);

//type for turns
typedef struct {
    unsigned short startPos;
    unsigned short endPos;
} TTurn;

//тип для результата хода (корректность)
//int, 0-fail, 1-success, 2-correct&atefigure

//тип для описания конкретной позиции
typedef struct {
    enum TFigureType {none, pawn, rook, king, bishop, queen, knight} type;
    unsigned short user; //0 - noone handles, 1 - first player, 2 - second player
} TDisposition;

//тип для описания конкретной позиции
typedef struct {
    TDisposition d[8][8];
} TGame;

//common type for messages
typedef struct {
    enum Type {login, logout, start, turn, disposition, result, userlist, log} type;
	size_t size;
	void* data;
} MessageType;

typedef struct {
    char name[MAX_NAME_LENGTH]; //static array to use sizeof
    int ownLevel;
    int desiredLevel;
    int id;
    int request;
} UserData;

MessageType composeMessage(enum Type type, size_t size, void* data);
int getMessage(int sd, MessageType* m);
int sendMessage(int sd, MessageType* m);

#endif //MESSAGES_H
