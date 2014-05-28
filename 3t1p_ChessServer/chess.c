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
        game->d[0][i].user = 2;
        game->d[1][i].user = 2;

        game->d[6][i].type = pawn;
        game->d[7][i].user = 1;
        game->d[6][i].user = 1;
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

short int checkTurn(TTurn* user_turn, TGame* game, int user_number)
{
    short int opp = 3 - user_number;
    short int start_letter = user_turn->startPos % 8;
    short int end_letter   = user_turn->endPos   % 8;
    short int start_number = 7 - (user_turn->startPos / 8);
    short int end_number   = 7 - (user_turn->endPos   / 8);
    fprintf(stderr, "%c%d -> %c%d\n", 'a'+start_letter, start_number, 'a'+end_letter, end_number);
    short int direction = (user_number * 2) - 3;
    TDisposition* start_disposition = &(game->d[start_number][start_letter]);
    TDisposition* end_disposition   = &(game->d[end_number  ][end_letter  ]);
    if (start_disposition->user != user_number || end_disposition->user == user_number){
        //it is not our piece at the start OR there is our piece at the end
        return TURN_INCORRECT;
    }
    switch (start_disposition->type){
    case pawn:
        if ((end_letter == start_letter) && (end_disposition->user == 0) &&
                ((direction * (end_number - start_number) == 1))){
            //forward step
            return TURN_CORRECT;
        }
        if ((abs(end_letter - start_letter) == 1) && (end_disposition->user == opp) &&
                (direction * (end_number - start_number) == 1)){
            //diagonal step (with capture)
            return TURN_CORRECT;
        }
        if ((end_letter == start_letter) && (end_disposition->user == 0) &&
                (direction * (end_number - start_number) == 2) &&
                ((start_number == 1) || (start_number == 6))){
            //advance (double step)
            short int num_dir = (end_number - start_number) / abs(end_number - start_number);
            short int number = start_number + num_dir;
            if (game->d[number][start_letter].user != 0){
                //there are pieces on the path
                return TURN_INCORRECT;
            }
            return TURN_CORRECT;
        }
        return TURN_INCORRECT;
    case knight:
        if ((abs(end_letter - start_letter) == 1) && (abs(end_number - start_number) == 2)){
            // 2 vert + 1 hor
            return TURN_CORRECT;
        }
        if ((abs(end_letter - start_letter) == 2) && (abs(end_number - start_number) == 1)){
            // 1 vert + 2 hor
            return TURN_CORRECT;
        }
        return TURN_INCORRECT;
    case bishop:
        if ((abs(end_letter - start_letter) == abs(end_number - start_number))){
            short int let_dir = (end_letter - start_letter) / abs(end_letter - start_letter);
            short int num_dir = (end_number - start_number) / abs(end_number - start_number);
            short int letter = start_letter + let_dir;
            short int number = start_number + num_dir;
            for (; letter != end_letter || number != end_number;){
                if (game->d[number][letter].user != 0){
                    //there are pieces on the path
                    return TURN_INCORRECT;
                }
                letter += let_dir;
                number += num_dir;
            }
            return TURN_CORRECT;
        }
        return TURN_INCORRECT;
    case rook:
        if (end_letter == start_letter || end_number == start_number){
            short int let_dir = end_letter - start_letter;
            short int num_dir = end_number - start_number;
            if (end_letter != start_letter){
                let_dir /= abs(end_letter - start_letter);
            }
            if (end_number != start_number){
                num_dir /= abs(end_number - start_number);
            }
            short int letter = start_letter + let_dir;
            short int number = start_number + num_dir;
            for (; letter != end_letter || number != end_number;){
                if (game->d[number][letter].user != 0){
                    //there are pieces on the path
                    return TURN_INCORRECT;
                }
                letter += let_dir;
                number += num_dir;
            }
            return TURN_CORRECT;
        }
        return TURN_INCORRECT;
    case queen:
        if (end_letter == start_letter || end_number == start_number ||
                abs(end_letter - start_letter) == abs(end_number - start_number)){
            short int let_dir = end_letter - start_letter;
            short int num_dir = end_number - start_number;
            if (end_letter != start_letter){
                let_dir /= abs(end_letter - start_letter);
            }
            if (end_number != start_number){
                num_dir /= abs(end_number - start_number);
            }
            short int letter = start_letter + let_dir;
            short int number = start_number + num_dir;
            for (; letter != end_letter || number != end_number;){
                if (game->d[number][letter].user != 0){
                    //there are pieces on the path
                    return TURN_INCORRECT;
                }
                letter += let_dir;
                number += num_dir;
            }
            return TURN_CORRECT;
        }
        return TURN_INCORRECT;
    case king:
        if (abs(end_letter - start_letter) <= 1 && abs(end_number - start_number) <= 1){
            return TURN_CORRECT;
        }
        return TURN_INCORRECT;
    default:
        return TURN_INCORRECT;
    }

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
