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
            users->list[(*user_id)].ownLevel = 0;
            users->list[(*user_id)].desiredLevel = 0;
            --(users->q);
            //close(incomeSd);
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

                //answer_m = composeMessage(start, sizeof(opponents[opponent_id]->name), &(opponents[opponent_id]->name[0]));
                answer_m = composeMessage(start, sizeof(UserData), opponents[opponent_id]);
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
        case log:
        {
            //read from file
        }
	}
}

MessageType* buf;
TGame* game;

void childSignalHandler(int signalVal)
{
    if (signalVal == SIGTERM){
        fprintf(stderr, "ServerChild terminates!\n");
        free(buf);
        free(game);
    }
    exit(EXIT_FAILURE);
}

void* establishConnection(int incomeSd, void* arg)
{
    signal(SIGTERM, childSignalHandler);
    UserList* users = (UserList*)arg;
	size_t user_id;
    buf = (MessageType*)malloc(sizeof(MessageType));
    game = NULL;
	while (1){
		getMessage(incomeSd, buf);
        //printf("I've got a message: %d\n", buf->type);
        readMessage(buf, incomeSd, &user_id, users, game);
		if (buf->type == logout){
			break;
        }
	}
    free(game);
	free(buf);
    //close(incomeSd);
	return NULL;
}

void* wait_child(void* arg)
{
    pid_t child_pid = *((pid_t*)arg);
    int status;
    waitpid(child_pid, &status, 0);
    return NULL;
}

UserList* users;
size_t total_users;
pid_t manager[MAX_TOTAL_USERS];
pthread_t son_waiter_thread[MAX_TOTAL_USERS];
int sd;


void parentSignalHandler(int signalVal)
{
    if (signalVal == SIGINT){
        fprintf(stderr, "Server terminates!\n");
        munmap(users, SHM_SIZE);
        shm_unlink(SHM_NAME);
        for (int i = 0; i < total_users; ++i){
            kill(manager[i], SIGTERM);
        }
        for (int i = 0; i < total_users; ++i){
            pthread_join(son_waiter_thread[i], NULL);
        }
        close(sd);
        unlink(SOCKNAME);
        exit(EXIT_SUCCESS);
    } else {
        exit(EXIT_FAILURE);
    }
}

void* manageConnections(void* arg)
{
    //Socket stuff
    /**/unlink(SOCKNAME);

    sd = socket(AF_UNIX, SOCK_STREAM, 0);
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

    users->q = 0;
    for (int i = 0; i < MAX_USERS; ++i){
        strcpy(users->list[i].name, "");
        users->list[i].ownLevel = 0;
        users->list[i].desiredLevel = 0;
        users->busy[i] = 0;
    }

    total_users = 0;
    while (total_users < MAX_TOTAL_USERS){
		int incomeSd;
		while ((incomeSd = accept(sd, 0, 0)) == -1);
        ++users->q;
        if (users->q > MAX_USERS){
            printf("Max number of connections reached!\n");
            while (users->q >= MAX_USERS){
                usleep(PAUSE_LENGTH);
            }
        } else {
            ++total_users;
            manager[total_users] = fork();
            if (manager[total_users] == 0){
                establishConnection(incomeSd, users);
                close(incomeSd);
                exit(EXIT_SUCCESS);
            } else {
                //wait for child using thread
                pthread_create(&son_waiter_thread[total_users], 0, wait_child, &manager[total_users]);
            }
        }
	}
    //this place will hardly be reached
    for (int i = 0; i < total_users; ++i){
        pthread_join(son_waiter_thread[i], NULL);
    }
	close(sd);
    unlink(SOCKNAME);
    return NULL;
}

int main()
{
    signal(SIGINT, parentSignalHandler);
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
    users = mmap(NULL, SHM_SIZE, PROT_WRITE|PROT_READ, MAP_SHARED, shm_fd, 0);

    pthread_t thread;
    pthread_create(&thread, 0, manageConnections, users);
    pthread_join(thread, 0);
    fprintf(stderr, "Server terminates!\n");

    munmap(users, SHM_SIZE);
    shm_unlink(SHM_NAME);

    return 0;
}
