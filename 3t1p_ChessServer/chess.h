#ifndef CHESS_H
#define CHESS_H
#include "messages.h"

void createChessboard(TGame* game);
short int checkTurn(char* turn_str);
void applyTurn(char* turn_str, TGame* game);

#endif //CHESS_H
