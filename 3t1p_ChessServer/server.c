#include "server.h"
User* users[MAX_USERS];
size_t user_q;

void readMessage(MessageType* m, int incomeSd, size_t* user_id)
{
	switch (m->type){
        case login:
		{
			UserData* ud = (UserData*)(m->data);
			size_t id = 0;
			while (id < MAX_USERS && users[id] != NULL){
				++id;
			}
            users[id] = ud;
			printf("Registered user #%zu. Name: %s. Level: %d. OpponentLevel: %d.\n", id, 
				users[id]->name, users[id]->ownLevel, users[id]->desiredLevel);
			(*user_id) = id;
			m->size = sizeof(size_t);
			m->data = &id;
			sendMessage(incomeSd, m);
			break;
		}
		case logout:
		{
			free(users[(*user_id)]);
			users[(*user_id)] = NULL;
			--user_q;
			close(incomeSd);
            printf("User #%zu logged out!\n", (*user_id));
			break;
		}
		case turn:
		{
			break;
		}
		case disposition:
		{
			break;
		}
		case result:
		{
			break;
		}
		case userlist:
		{
            //create opponent list
            User* opponents[MAX_USERS];
            size_t opponent_q = 0;
            for (int i = 0; i < user_q; ++i){
                if (users[i]->ownLevel == users[*(user_id)]->desiredLevel && i != *(user_id)){
                    ++opponent_q;
                    opponents[opponent_q-1] = users[i];
                }
            }
            MessageType answer_m = composeMessage(userlist, opponent_q * sizeof(User*), &(opponents[0]));
            sendMessage(incomeSd, &answer_m);
            break;
		}
	}
}

void* establishConnection(void* arg)
{
	size_t user_id;
	int incomeSd = *((int*)arg);
	MessageType* buf = (MessageType*)malloc(sizeof(MessageType));
	while (1){
		getMessage(incomeSd, buf);
		readMessage(buf, incomeSd, &user_id);
		if (buf->type == logout){
			break;
		}
	}
	free(buf);
	return NULL;
}

void* manageConnections(void* arg)
{
	/**/unlink(SOCKNAME);

	int sd = socket(AF_UNIX, SOCK_STREAM, 0);
	if (sd == -1){
		throwError("server: socket");
	}
	struct sockaddr_un server_addr;
	server_addr.sun_family = AF_UNIX;
	strcpy(server_addr.sun_path, SOCKNAME);

	int call_result = 0;
	call_result = bind(sd, (struct sockaddr*) &server_addr, sizeof(struct sockaddr_un));
	if (call_result == -1){
		throwError("server: bind");
	}
	call_result = listen(sd, MAX_LISTENED);
	if (call_result == -1){
		throwError("server: listen");
	}
	pthread_t thread[MAX_USERS];
	user_q = 0;
	while (1){
		int incomeSd;
		while ((incomeSd = accept(sd, 0, 0)) == -1);
		++user_q;
		pthread_create(&thread[user_q-1], 0, establishConnection, &incomeSd);
        if (user_q >= MAX_USERS){
            printf("Max number of connections reached!\n");
            while (user_q >= MAX_USERS){
                usleep(PAUSE_LENGTH);
            }
        }
	}
    //this place will not be reached
	for (int i = 0; i < user_q; ++i){
		pthread_join(thread[i], 0);
	}
	close(sd);
	unlink(SOCKNAME);
    return NULL;
}

int main()
{
	for (int i = 0; i < MAX_USERS; ++i){
		users[i] = NULL;
	}

	pthread_t thread;
	pthread_create(&thread, 0, manageConnections, 0);
	pthread_join(thread, 0);
	fprintf(stderr, "Server terminates!\n");
	return 0;
}
