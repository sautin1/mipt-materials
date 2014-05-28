#ifndef CLIENT_H
#define CLIENT_H

//Colors for disposition:
#define KNRM  "\x1B[0m"
#define KRED  "\x1B[31m"
#define KGRN  "\x1B[32m"
#define KYEL  "\x1B[33m"
#define KBLU  "\x1B[34m"
#define KMAG  "\x1B[35m"
#define KCYN  "\x1B[36m"
#define KWHT  "\x1B[37m"

void ask_login(int sd, int* user_id);
void ask_logout(int sd);
void ask_userlist(int sd);
void ask_turn(int sd);
void ask_disposition(int sd);
int connectServer();
void startGame(int sd);

#endif //CLIENT_H
