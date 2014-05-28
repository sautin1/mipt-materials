#ifndef CLIENT_H
#define CLIENT_H

#include "messages.h"
#include "common.h"

//Colors for disposition:
#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KGRN  "\x1B[32m"
#define KYEL  "\x1B[33m"
#define KBLU  "\x1B[34m"
#define KMAG  "\x1B[35m"
#define KCYN  "\x1B[36m"
#define KWHT  "\x1B[37m"

void ask_login(int sd, int* user_id, short int* user_color);
void ask_logout(int sd, short int user_color);
void ask_userlist(int sd);

TTurn* encodeTurn(char* turn_str);
char* decodeTurn(TTurn* turn_code);
void reverseTurn(TTurn* user_turn);
void ask_turn(int sd, short int user_color);

void ask_disposition(int sd, short int user_color);
void ask_log(int sd, short int user_color);
int connectServer();
void startGame(int sd);

#endif //CLIENT_H
