#include "server.h"
User* users[MAX_USERS];
size_t user_q;

void readMessage(MessageType* m, int incomeSd, size_t* user_id)
{
	switch (m->type){
		case login: //добавить пользователя и найти ему пару
		{
			UserData* ud = (UserData*)(m->data);
			if (user_q >= MAX_USERS){
				throwError("player overflow");
			}
			size_t id = 0;
			while (id < MAX_USERS && users[id] != NULL){
				++id;
			}
			users[id] = ud; //ud никуда не денется, потому что в куче
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
            printf("User #%zu logged out!\n", (*user_id));
			free(users[(*user_id)]);
			users[(*user_id)] = NULL;
			--user_q;
			close(incomeSd);
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
			User** opponents = (User**)malloc(sizeof(User*) * MAX_USERS);
			size_t opponent_q = 0;
			for (int i = 0; i < user_q; ++i){
				if (users[i]->ownLevel == users[*(user_id)]->desiredLevel && i != *(user_id)){
					++opponent_q;
					opponents[opponent_q-1] = users[i];
				}
			}
			MessageType answer_m;
			answer_m.type = userlist;
			answer_m.size = opponent_q;
			answer_m.data = opponents;

			sendMessage(incomeSd, &answer_m);
			free(opponents);
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
	/**/size_t connection_q = 0;
	while (1){
		int incomeSd;
		while ((incomeSd = accept(sd, 0, 0)) == -1);
		++user_q;
		/**/++connection_q;
		pthread_create(&thread[user_q-1], 0, establishConnection, &incomeSd);
		/**/if (connection_q >= MAX_USERS){
			break;
		}
	}
	for (int i = 0; i < user_q; ++i){
		pthread_join(thread[i], 0);
	}
	close(sd);
	unlink(SOCKNAME);
	return NULL; //ибо void*
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
