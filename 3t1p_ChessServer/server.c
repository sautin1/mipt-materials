#include "server.h"
//int userGame[MAX_USERS];
//TGame* g = (TGame*)calloc(sizeof(Game) * MAX_GAMES);

size_t createOpponentList(void* arg1, void* arg2, size_t user_id)
{
    UserList* users = (UserList*)arg1;
    User** opponents = (User**)arg2;
    size_t opponent_q = 0;
    for (int i = 0; i < MAX_USERS; ++i){
        if (users->list[i].ownLevel != 0 && !(users->busy[i]) &&
            users->list[i].ownLevel == users->list[user_id].desiredLevel &&
            users->list[i].desiredLevel == users->list[user_id].ownLevel && i != user_id){
            ++opponent_q;
            opponents[opponent_q-1] = &(users->list[i]);
        }
    }
    return opponent_q;
}

void readMessage(MessageType* m, int incomeSd, size_t* user_id, void* arg1, void* arg2)
{
    UserList* users = (UserList*)arg1;
    TGame* game = (TGame*)arg2;
	switch (m->type){
        case login:
		{
			UserData* ud = (UserData*)(m->data);
			size_t id = 0;
            while (id < MAX_USERS && users->list[id].ownLevel != 0){
				++id;
			}
            memcpy(&users->list[id], ud, sizeof(User));
            free(ud);
			printf("Registered user #%zu. Name: %s. Level: %d. OpponentLevel: %d.\n", id, 
                users->list[id].name, users->list[id].ownLevel, users->list[id].desiredLevel);
			(*user_id) = id;
            users->list[id].id = id;
            MessageType answer_m = composeMessage(login, sizeof(size_t), &id);
            sendMessage(incomeSd, &answer_m);
			break;
		}
		case logout:
		{
            strcpy(users->list[(*user_id)].name, "");
            --(users->q);
			close(incomeSd);
            free(game);
            printf("User #%zu logged out!\n", (*user_id));
			break;
		}
        case start:
        {
            //create opponent list
            User* opponents[MAX_USERS];
            size_t opponent_q = createOpponentList(users, &(opponents[0]), *user_id);
            MessageType answer_m;
            if (opponent_q == -1){
                answer_m = composeMessage(start, 0, NULL);
            } else {
                size_t opponent_id = rand() % opponent_q;
                users->busy[*user_id] = 1;
                users->busy[opponents[opponent_id]->id] = 1;
                answer_m = composeMessage(start, sizeof(opponents[opponent_id]->name), &(opponents[opponent_id]->name[0]));
            }
            sendMessage(incomeSd, &answer_m);
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
            size_t opponent_q = createOpponentList(users, &(opponents[0]), *user_id);

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
    TGame* game = NULL;
	while (1){
		getMessage(incomeSd, buf);
        //printf("I've got a message: %d\n", buf->type);
        readMessage(buf, incomeSd, &user_id, users, game);
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
        users->list[i].ownLevel = 0;
        users->list[i].desiredLevel = 0;
        users->busy[i] = 0;
    }
    pid_t manager[MAX_USERS] = {0};
	while (1){
		int incomeSd;
		while ((incomeSd = accept(sd, 0, 0)) == -1);
        ++users->q;
        size_t manager_index = 0;
        while (manager_index < MAX_USERS && manager[manager_index] != 0){
            ++manager_index;
        }
        if (manager_index == MAX_USERS){
            printf("Max number of connections reached!\n");
            while (users->q >= MAX_USERS){
                usleep(PAUSE_LENGTH);
            }
        } else {
            manager[manager_index] = fork();
            if (manager[manager_index] == 0){
                establishConnection(incomeSd, users);
                exit(EXIT_SUCCESS);
            } else {
                //wait for child using thread. After child terminates make manager[manager_index] = 0
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
