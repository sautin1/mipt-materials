#ifndef CHESS_H
#define CHESS_H
#include "messages.h"

void createChessboard(TGame* game);
short int checkTurn(TTurn* user_turn);
void applyTurn(TTurn* user_turn, TGame* game);

#endif //CHESS_H
