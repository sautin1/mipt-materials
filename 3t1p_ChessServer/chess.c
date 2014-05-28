#include "chess.h"

void createChessboard(TGame* game)
{
    game->user = 1;
    for (int i = 0; i < 8; ++i){
        for (int j = 0; j < 8; ++j){
            game->d[i][j].type = none;
            game->d[i][j].user = 0;
        }
    }

    for (int i = 0; i < 8; ++i){
        game->d[1][i].type = pawn;
        game->d[0][i].user = 1;
        game->d[1][i].user = 1;

        game->d[6][i].type = pawn;
        game->d[7][i].user = 2;
        game->d[6][i].user = 2;
    }
    game->d[0][0].type = rook;
    game->d[0][1].type = knight;
    game->d[0][2].type = bishop;
    game->d[0][3].type = queen;
    game->d[0][4].type = king;
    game->d[0][5].type = bishop;
    game->d[0][6].type = knight;
    game->d[0][7].type = rook;

    game->d[7][0].type = rook;
    game->d[7][1].type = knight;
    game->d[7][2].type = bishop;
    game->d[7][3].type = queen;
    game->d[7][4].type = king;
    game->d[7][5].type = bishop;
    game->d[7][6].type = knight;
    game->d[7][7].type = rook;
}

short int checkTurn(TTurn* user_turn)
{
    return 0;
}

void applyTurn(TTurn* user_turn, TGame* game)
{
    TDisposition* start_disposition = &(game->d[7 - user_turn->startPos / 8][user_turn->startPos % 8]);
    TDisposition* end_disposition   = &(game->d[7 - user_turn->endPos   / 8][user_turn->endPos   % 8]);
    memcpy(end_disposition, start_disposition, sizeof(TDisposition));
    start_disposition->type = none;
    start_disposition->user = 0;
}
