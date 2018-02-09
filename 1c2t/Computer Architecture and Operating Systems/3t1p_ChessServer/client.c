#include "client.h"

void ask_login(int sd, int* user_id, short int* user_color)
{
	UserData ud;
	printf("\tEnter your name (max length %d): ", MAX_NAME_LENGTH);
	scanf("%s", ud.name);
	ud.ownLevel = 0;
	while (ud.ownLevel <= 0 || ud.ownLevel > MAX_LEVEL){
		printf("\tEnter your level (max %d): ", MAX_LEVEL);
		scanf("%d", &ud.ownLevel);
	}
	ud.desiredLevel = 0;
	while (ud.desiredLevel <= 0 || ud.desiredLevel > MAX_LEVEL){
		printf("\tEnter your opponent's level (max %d): ", MAX_LEVEL);	
		scanf("%d", &ud.desiredLevel);
	}
    ud.id = 0;

	MessageType m;
	m = composeMessage(login, sizeof(UserData), &ud);
	sendMessage(sd, &m);
    getMessage(sd, &m); //get new user's index
	(*user_id) = *((size_t*)m.data);
	printf("Logged in!\n\t%s, your user_id is %d\n", ud.name, *user_id);

    printf("Looking for a partner...\n");
    m = composeMessage(start, 0, NULL);
    sendMessage(sd, &m);
    getMessage(sd, &m); //failure/success message (with opponent UserData)
    if (m.size == 0){
        printf("No players with desired level! Please wait...\n");
        getMessage(sd, &m); //success message (with opponent UserData)
    }
    UserData* opponent = (UserData*)(m.data);
    printf("Game with player #%d %s started!\n", opponent->id, opponent->name);
    free(opponent);
    getMessage(sd, &m); //white/black
    *user_color = *(int*)(m.data);
    if (*user_color == 1){
        printf("You're White. White goes first!\n");
    } else {
        printf("You're Black. Black goes second!\n");
    }
}

void ask_log(int sd, short int user_color)
{
    MessageType m;
    m = composeMessage(log, 0, NULL);
    sendMessage(sd, &m);
    while (1){
        getMessage(sd, &m);
        if (m.size == 0){
            break;
        } else {
            TTurn* user_turn = (TTurn*)m.data;
            if (user_color != 1){
                //black
                reverseTurn(user_turn);
            }
            char* turn_str = decodeTurn(user_turn);
            printf("%s\n", turn_str);
            free(m.data);
        }
    }
}

void ask_logout(int sd, short int user_color)
{
    char answer;
    printf("\tWould you like to see the log of the game? (y/n) ");
    scanf(" %c", &answer);
    if (answer != 'n'){
        printf("\n\nCurrent disposition:\n");
        ask_disposition(sd, user_color);
        printf("Game log:\n");
        ask_log(sd, user_color);
    }
    MessageType m;
    m = composeMessage(logout, 0, NULL);
    sendMessage(sd, &m);
}

void ask_userlist(int sd)
{
	MessageType m;
	m = composeMessage(userlist, 0, NULL);
	sendMessage(sd, &m);
	getMessage(sd, &m);
	if (m.size == 0){
        printf("No players online!\n");
	}
    size_t user_q = m.size / sizeof(UserData*);
    for (int i = 0; i < user_q; ++i){
        UserData* user = ((UserData**)(m.data))[i];
        printf("Player %d.\n\tName: %s\n\tOwnLevel: %d\n\tDesiredLevel: %d\n___\n", i, user->name, user->ownLevel, user->desiredLevel);
        free(user);
	}
	free(m.data);
}

TTurn* encodeTurn(char* turn_str)
{
    TTurn* turn_code = (TTurn*)malloc(sizeof(TTurn));
    turn_code->startPos = tolower(turn_str[0]) - 'a' + 8*(turn_str[1] - '1');
    turn_code->endPos   = tolower(turn_str[2]) - 'a' + 8*(turn_str[3] - '1');
    return turn_code;
}

char* decodeTurn(TTurn* turn_code)
{
    char* turn_str = (char*)malloc(TURN_LENGTH);
    turn_str[0] = ((turn_code->startPos % 8) + 'a');
    turn_str[1] = ((turn_code->startPos / 8) + '1');
    turn_str[2] = ((turn_code->endPos   % 8) + 'a');
    turn_str[3] = ((turn_code->endPos   / 8) + '1');
    return turn_str;
}

void reverseTurn(TTurn* user_turn)
{
    user_turn->startPos = (8 - (user_turn->startPos / 8 + 1)) * 8 + ('h' - 'a' - (user_turn->startPos % 8));
    user_turn->endPos   = (8 - (user_turn->endPos   / 8 + 1)) * 8 + ('h' - 'a' - (user_turn->endPos   % 8));
}

void ask_turn(int sd, short int user_color)
{
    char turn_str[5] = {0};
    while ((turn_str[0] < 'a') || (turn_str[0] > 'h') ||
           (turn_str[2] < 'a') || (turn_str[2] > 'h') ||
           (turn_str[1] < '1') || (turn_str[1] > '8') ||
           (turn_str[3] < '1') || (turn_str[3] > '8') ||
           (turn_str[4] != 0 ) ||
           ((turn_str[0] == turn_str[2]) && (turn_str[1] == turn_str[3]))){
        printf("\tYour turn (for example, a2a4): ");
        scanf(" %s", turn_str);
    }
    turn_str[4] = '\0';
    TTurn* user_turn = encodeTurn(turn_str);
    if (user_color != 1){
        //black
        reverseTurn(user_turn);
    }
    MessageType m;
    m = composeMessage(turn, sizeof(TTurn), user_turn);
    sendMessage(sd, &m);
    //free(m.data);
    getMessage(sd, &m);
    int result = *((int*)m.data);
    switch (result){
        case TURN_CORRECT:
        {
            printf("Turn accepted!\n");
            break;
        }
        case TURN_NOT_TIME:
        {
            printf("It is not your time to make turns!\n");
            break;
        }
        default:
        {
            printf("Turn is incorrect! Try again!\n");
            break;
        }
    }
}

void ask_disposition(int sd, short int user_color)
{
    MessageType m;
    m = composeMessage(disposition, 0, NULL);
    sendMessage(sd, &m);
    getMessage(sd, &m);
    TGame* board = (TGame*)m.data;
    char* text_color[3] = {KNRM, KCYN, KRED};
    for (int i = 0; i < 8; ++i){
        printf("%d | ", 8-i);
        for (int j = 0; j < 8; ++j){
            if (user_color != 1){
                //black
                printf("%s%d ", text_color[board->d[7-i][7-j].user], board->d[7-i][7-j].type);
            } else {
                //white
                printf("%s%d ", text_color[board->d[i][j].user], board->d[i][j].type);
            }
        }
        printf("%s\n", KNRM);
    }
    printf("   ________________\n   ");
    for (int i = 0; i < 8; ++i){
        printf(" %c", 'A' + i);
    }
    printf("\n");
}

void startGame(int sd)
{
    int user_id = -1;
    short int user_color;
    short int game_started;
    while (1){
        char* command = (char*)malloc(MAX_NAME_LENGTH);
        printf(">> ");
        scanf("%s", command);
        if (strcmp(command, "login") == 0){
            if (user_id == -1){
                printf("Logging in...\n");
                ask_login(sd, &user_id, &user_color);
            } else {
                printf("You're logged in!\n");
            }
            game_started = 1;
        }
        if (strcmp(command, "logout") == 0){
            printf("Logging out...\n");
            ask_logout(sd, user_color);
            break;
        }
        if (strcmp(command, "userlist") == 0){
            ask_userlist(sd);
        }
        if (strcmp(command, "disposition") == 0){
            if (game_started == 0){
                printf("Login first!\n");
                continue;
            }
            ask_disposition(sd, user_color);
        }
        if (strcmp(command, "turn") == 0){
            if (game_started == 0){
                printf("Login first!\n");
                continue;
            }
            ask_turn(sd, user_color);
        }
        if (strcmp(command, "log") == 0){
            if (game_started == 0){
                printf("Login first!\n");
                continue;
            }
            ask_log(sd, user_color);
        }
        free(command);
    }
}

int connectServer()
{
    int sd = socket(AF_INET, SOCK_STREAM, 0);
    if (sd == -1){
        throwError("Client: socket cannot be created");
    }
    struct sockaddr_in client_addr;
    client_addr.sin_family = AF_INET;
    //strcpy(client_addr.sun_path, SOCKNAME);
    client_addr.sin_port = htons(PORT);
    inet_aton("127.0.0.1", (struct in_addr*)&client_addr.sin_addr.s_addr);

    int call_result = 0;
    call_result = connect(sd, (struct sockaddr*)&client_addr, sizeof(struct sockaddr_in));
    if (call_result == -1){
        throwError("Client: connect error");
    }
    return sd;
}

int main()
{
    int sd = connectServer();
    startGame(sd);
    close(sd);
	return 0;
}
