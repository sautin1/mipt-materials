#include "server.h"

void readMessage(MessageType* m, int incomeSd, size_t* user_id, void* arg)
{
    UserList* users = (UserList*)arg;
	switch (m->type){
        case login:
		{
			UserData* ud = (UserData*)(m->data);
			size_t id = 0;
            while (id < MAX_USERS && strcmp(users->list[id].name, "")){
				++id;
			}
            memcpy(&users->list[id], ud, sizeof(User));
            free(ud);
			printf("Registered user #%zu. Name: %s. Level: %d. OpponentLevel: %d.\n", id, 
                users->list[id].name, users->list[id].ownLevel, users->list[id].desiredLevel);
			(*user_id) = id;
            MessageType answer_m = composeMessage(login, sizeof(size_t), &id);
            sendMessage(incomeSd, &answer_m);
			break;
		}
		case logout:
		{
            strcpy(users->list[(*user_id)].name, "");
            --users->q;
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
            for (int i = 0; i < users->q; ++i){
                if (users->list[i].ownLevel == users->list[*(user_id)].desiredLevel && i != *(user_id)){
                    ++opponent_q;
                    opponents[opponent_q-1] = &(users->list[i]);
                }
            }
            MessageType answer_m = composeMessage(userlist, opponent_q * sizeof(User*), &(opponents[0]));
            sendMessage(incomeSd, &answer_m);
            break;
		}
	}
}

void* establishConnection(int incomeSd, void* arg)
{
    UserList* users = (UserList*)arg;
	size_t user_id;
	MessageType* buf = (MessageType*)malloc(sizeof(MessageType));
	while (1){
		getMessage(incomeSd, buf);
        readMessage(buf, incomeSd, &user_id, users);
		if (buf->type == logout){
			break;
        }
	}
	free(buf);
	return NULL;
}

void* manageConnections(void* arg)
{
    //Socket stuff
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
    //_______________

    UserList* users = (UserList*)arg;
    users->q = 0;
    for (int i = 0; i < MAX_USERS; ++i){
        strcpy(users->list[i].name, "");
    }
    pid_t manager[MAX_USERS];
	while (1){
		int incomeSd;
		while ((incomeSd = accept(sd, 0, 0)) == -1);
        ++users->q;
        manager[users->q - 1] = fork();
        if (manager[users->q - 1] == 0){
            establishConnection(incomeSd, users);
            exit(EXIT_SUCCESS);
        }
        if (users->q >= MAX_USERS){
            printf("Max number of connections reached!\n");
            while (users->q >= MAX_USERS){
                usleep(PAUSE_LENGTH);
            }
        }
	}
    //this place will not be reached
    for (int i = 0; i < users->q; ++i){
        int status;
        waitpid(manager[i], &status, 0);
	}
	close(sd);
    unlink(SOCKNAME);
    return NULL;
}

int main()
{
    int shm_fd;
    shm_fd = shm_open(SHM_NAME, O_RDWR|O_CREAT|O_EXCL, 0666);
    if (shm_fd == -1){
        shm_unlink(SHM_NAME);
        shm_fd = shm_open(SHM_NAME, O_RDWR|O_CREAT|O_EXCL, 0666);
        if (shm_fd == -1){
            throwError("server: shm_open");
        }
    }

    ftruncate(shm_fd, SHM_SIZE);
    UserList* users = mmap(NULL, SHM_SIZE, PROT_WRITE|PROT_READ, MAP_SHARED, shm_fd, 0);

    pthread_t thread;
    pthread_create(&thread, 0, manageConnections, users);
    pthread_join(thread, 0);
    fprintf(stderr, "Server terminates!\n");

    munmap(users, SHM_SIZE);
    shm_unlink(SHM_NAME);

    return 0;
}
