#include "messages.h"
#include "common.h"

void logIn(int sd, int* user_id)
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

	MessageType m;
	m = composeMessage(login, sizeof(UserData), &ud);
	sendMessage(sd, &m);
    getMessage(sd, &m); //here we'll get new user's index
	(*user_id) = *((size_t*)m.data);
	printf("Logged in!\n\t%s, your user_id is %d\n", ud.name, *user_id);
}

void logOut(int sd)
{
	MessageType m;
	m = composeMessage(logout, 0, NULL);
	sendMessage(sd, &m);
	close(sd);
}

void userList(int sd)
{
	MessageType m;
	m = composeMessage(userlist, 0, NULL);
	sendMessage(sd, &m);
	getMessage(sd, &m);
	if (m.size == 0){
		printf("No players with desired level!\n");
	}
	for (int i = 0; i < m.size; ++i){
		UserData* opponent = ((UserData**)(m.data))[i];
		printf("Player %d.\n\tName: %s\n\tOwnLevel: %d\n\tDesiredLevel: %d\n___\n", i, opponent->name, opponent->ownLevel, opponent->desiredLevel);
		free(((UserData**)(m.data))[i]);
	}
	free(m.data);
}

int main()
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

    int user_id = -1;
	while (1){
		char* command = (char*)malloc(MAX_NAME_LENGTH);
		printf(">> ");
		scanf("%s", command);
		if (strcmp(command, "login") == 0){
			if (user_id == -1){
				printf("Logging in...\n");
				logIn(sd, &user_id);
			} else {
				printf("You're logged in!\n");
			}
		}
		if (strcmp(command, "logout") == 0){
			printf("Logging out...\n");	
            logOut(sd);
			break;
		}
		if (strcmp(command, "userlist") == 0){
			userList(sd);
		}
		free(command);
	}
	//close(sd);
	return 0;
}
