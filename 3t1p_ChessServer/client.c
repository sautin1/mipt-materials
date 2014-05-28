#include "client.h"

void ask_login(int sd, int* user_id)
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
    if (*(int*)(m.data) == 1){
        printf("You're White. White goes first!\n");
    } else {
        printf("You're Black. Black goes second!\n");
    }
}

void ask_log(int sd)
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
            char* turn_str = decodeTurn(user_turn);
            printf("%s\n", turn_str);
            free(m.data);
        }
    }
}

void ask_logout(int sd)
{
    char answer;
    printf("\tWould you like to see the log of the game? (y/n) ");
    scanf(" %c", &answer);
    if (answer != 'n'){
        printf("\n\nCurrent disposition:");
        ask_disposition(sd);
        printf("Game log:");
        ask_log(sd);
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


void ask_turn(int sd)
{
    while (1){
        char turn_str[5] = {0};
        while ((turn_str[0] < 'a' || turn_str[0] > 'h') ||
               (turn_str[2] < 'a' || turn_str[2] > 'h') ||
               (turn_str[1] < '1' || turn_str[1] > '8') ||
               (turn_str[3] < '1' || turn_str[3] > '8')){
            printf("\tYour turn (for example, a2a4): ");
            scanf(" %s", turn_str);
        }
        turn_str[4] = '\0';
        TTurn* user_turn = encodeTurn(turn_str);
        MessageType m;
        m = composeMessage(turn, sizeof(TTurn), user_turn);
        sendMessage(sd, &m);
        //free(m.data);
        getMessage(sd, &m);
        int result = *((int*)m.data);
        switch (result){
            case 0:
            {
                printf("Turn accepted!\n");
                break;
            }
            case 1:
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
        if (result == 0 || result == 1){
            break;
        }
    }
}

void ask_disposition(int sd)
{
    MessageType m;
    m = composeMessage(disposition, 0, NULL);
    sendMessage(sd, &m);
    getMessage(sd, &m);
    TGame* board = (TGame*)m.data;
    char* color[3] = {KNRM, KCYN, KRED};
    for (int i = 0; i < 8; ++i){
        printf("%d | ", 8-i);
        for (int j = 0; j < 8; ++j){
            printf("%s%d ", color[board->d[i][j].user], board->d[i][j].type);
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
    short int game_started;
    while (1){
        char* command = (char*)malloc(MAX_NAME_LENGTH);
        printf(">> ");
        scanf("%s", command);
        if (strcmp(command, "login") == 0){
            if (user_id == -1){
                printf("Logging in...\n");
                ask_login(sd, &user_id);
            } else {
                printf("You're logged in!\n");
            }
            game_started = 1;
        }
        if (strcmp(command, "logout") == 0){
            printf("Logging out...\n");
            ask_logout(sd);
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
            ask_disposition(sd);
        }
        if (strcmp(command, "turn") == 0){
            if (game_started == 0){
                printf("Login first!\n");
                continue;
            }
            ask_turn(sd);
        }
        if (strcmp(command, "log") == 0){
            if (game_started == 0){
                printf("Login first!\n");
                continue;
            }
            ask_log(sd);
        }
        free(command);
    }
}

int connectServer()
{
    int sd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sd == -1){
        throwError("Client: socket cannot be created");
    }
    struct sockaddr_un client_addr;
    client_addr.sun_family = AF_UNIX;
    strcpy(client_addr.sun_path, SOCKNAME);

    int call_result = 0;
    call_result = connect(sd, (struct sockaddr*) &client_addr, sizeof(struct sockaddr_un));
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
