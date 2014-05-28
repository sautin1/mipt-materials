#include "server.h"

char* getFilename(char* filename, size_t game_id)
{
    strcpy(filename, FILENAME_START);
    char* unique_id = (char*)malloc(UNIQUE_ID_LENGTH);
    sprintf(unique_id, "%zu", game_id);
    strcat(filename, unique_id);
    strcat(filename, FILENAME_END);
    return filename;
}

size_t createOpponentList(void* arg1, void* arg2, size_t user_id)
{
    UserGameList* users = (UserGameList*)arg1;
    User** opponents = (User**)arg2;
    size_t opponent_q = 0;
    for (int i = 0; i < MAX_USERS; ++i){
        if (users->list[i].ownLevel != 0 && users->color[i] == 0 &&
            users->list[i].ownLevel == users->list[user_id].desiredLevel &&
            users->list[i].desiredLevel == users->list[user_id].ownLevel && i != user_id){
            ++opponent_q;
            opponents[opponent_q-1] = &(users->list[i]);
        }
    }
    return opponent_q;
}

void clearUser(void* arg, int user_id)
{
    UserGameList* users = (UserGameList*)arg;
    users->list[user_id].ownLevel = 0;
    users->list[user_id].desiredLevel = 0;
    strcpy(users->list[user_id].name, "");
    users->list[user_id].request = user_id;
    users->color[user_id] = 0;
    users->usergame[user_id] = -1;
}

pthread_mutex_t mutex;

void readMessage(MessageType* m, int incomeSd, size_t* user_id, /*size_t* opponent_id,*/ void* arg)
{
    UserGameList* users = (UserGameList*)arg;
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
            users->list[id].request = id;
            MessageType answer_m = composeMessage(login, sizeof(size_t), &id);
            sendMessage(incomeSd, &answer_m);
			break;
		}
		case logout:
		{
            clearUser(users, *user_id);
            --(users->q);
            printf("User #%zu logged out!\n", (*user_id));
			break;
		}
        case start:
        {
            //create opponent list
            User* opponents[MAX_USERS];
            size_t opponent_q = createOpponentList(users, &(opponents[0]), *user_id);
            MessageType answer_m;
            size_t i;
            for (i = 0; i < opponent_q; ++i){
                pthread_mutex_lock(&mutex);
                if (users->color[opponents[i]->id] == 0){
                    users->color[opponents[i]->id] = 2;
                    users->color[*user_id] = 1;
                    size_t game_id;
                    for (game_id = 0; game_id < MAX_USERS; ++game_id){
                        if (users->game[game_id].user == 0){
                            users->usergame[*user_id] = game_id;
                            users->usergame[opponents[i]->id] = game_id;
                            break;
                        }
                    }
                    createChessboard(&(users->game[game_id]));
                    users->list[opponents[i]->id].request = *user_id;
                    answer_m = composeMessage(start, sizeof(UserData), opponents[i]);
                    pthread_mutex_unlock(&mutex);
                    break;
                }
                pthread_mutex_unlock(&mutex);
            }
            if (i == opponent_q){
                //wait
                //*opponent_id = i;
                answer_m = composeMessage(start, 0, NULL);
                sendMessage(incomeSd, &answer_m);

                while (users->list[*user_id].request == *user_id);
                i = users->list[*user_id].request;
                User* opponent = &(users->list[i]);
                answer_m = composeMessage(start, sizeof(User), opponent);
            }
            sendMessage(incomeSd, &answer_m);
            createChessboard(users->game);

            char* filename = (char*)malloc(FILENAME_LENGTH * sizeof(char));
            getFilename(filename, users->usergame[*user_id]);
            FILE* log_file = fopen(filename, "w");
            fclose(log_file);
            free(filename);

            answer_m = composeMessage(start, sizeof(int), &(users->color[*user_id]));
            sendMessage(incomeSd, &answer_m);
            break;
        }
		case turn:
		{
            MessageType answer_m;
            char* turn_str = (char*)m->data;
            int check;
            size_t game_id = users->usergame[*user_id];
            if (users->game[game_id].user != users->color[*user_id]){
                check = 1;
            } else {
                check = checkTurn(turn_str);
                if (check == 0){
                    //applyTurn(turn_str, &users->game[game_id]);
                    //Save turn to log file
                    char* filename = (char*)malloc(FILENAME_LENGTH * sizeof(char));
                    getFilename(filename, game_id);
                    FILE* log_file = fopen(filename, "a");
                    fprintf(log_file, "%s\n", turn_str);
                    fclose(log_file);
                    free(filename);
                }
                users->game[game_id].user = 3 - users->game[game_id].user;
            }
            answer_m = composeMessage(turn, sizeof(int), &check);
            sendMessage(incomeSd, &answer_m);
            free(turn_str);
            break;
		}
		case disposition:
		{
            MessageType answer_m;
            size_t game_id = users->usergame[*user_id];
            answer_m = composeMessage(disposition, sizeof(TGame), &(users->game[game_id]));
            sendMessage(incomeSd, &answer_m);
			break;
		}
        case result:
        {
            break;
        }
        case userlist:
		{
            User* active_users[users->q];
            size_t act_user_q = 0;
            for (int i = 0; i < MAX_USERS; ++i){
                if (users->list[i].ownLevel != 0){
                    ++act_user_q;
                    active_users[act_user_q-1] = &(users->list[i]);
                }
            }
            //create user list
            MessageType answer_m = composeMessage(userlist, act_user_q * sizeof(User*), &(active_users[0]));
            sendMessage(incomeSd, &answer_m);
            break;
		}
        case log:
        {
            //read from file
            char* filename = (char*)malloc(FILENAME_LENGTH * sizeof(char));
            getFilename(filename, users->usergame[*user_id]);
            FILE* log_file = fopen(filename, "r");
            if (log_file == NULL){
                throwError("server: log file");
            }
            MessageType answer_m;
            char* one_turn = (char*)malloc(TURN_LENGTH);
            while (1){
                int call_result = fscanf(log_file, "%s\n", one_turn);
                if (call_result == 0 || call_result == EOF){
                    answer_m = composeMessage(log, 0, NULL);
                    sendMessage(incomeSd, &answer_m);
                    break;
                } else {
                    one_turn[TURN_LENGTH-1] = '\0';
                    answer_m = composeMessage(log, TURN_LENGTH * sizeof(char), one_turn);
                    sendMessage(incomeSd, &answer_m);
                }
            }
            fclose(log_file);
            free(one_turn);
            free(filename);
        }
	}
}

MessageType* buf;

void childSignalHandler(int signalVal)
{
    if (signalVal == SIGTERM){
        fprintf(stderr, "ServerChild terminates!\n");
        free(buf);
    }
    exit(EXIT_FAILURE);
}

void* establishConnection(int incomeSd, void* arg)
{
    signal(SIGTERM, childSignalHandler);
    UserGameList* users = (UserGameList*)arg;
    size_t user_id;
    //size_t opponent_id;
    buf = (MessageType*)malloc(sizeof(MessageType));
	while (1){
		getMessage(incomeSd, buf);
        readMessage(buf, incomeSd, &user_id, /*&opponent_id,*/ users);
		if (buf->type == logout){
			break;
        }
	}
	free(buf);
	return NULL;
}

void* wait_child(void* arg)
{
    pid_t child_pid = *((pid_t*)arg);
    int status;
    waitpid(child_pid, &status, 0);
    return NULL;
}

UserGameList* users;
size_t total_users;
pid_t manager[MAX_TOTAL_USERS];
pthread_t child_waiter_thread[MAX_TOTAL_USERS];
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
            pthread_join(child_waiter_thread[i], NULL);
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
        clearUser(users, i);
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
                pthread_create(&child_waiter_thread[total_users], 0, wait_child, &manager[total_users]);
            }
        }
	}
    //this place will hardly be reached
    for (int i = 0; i < total_users; ++i){
        pthread_join(child_waiter_thread[i], NULL);
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
    //fprintf(stderr, "Server terminates!\n");

    munmap(users, SHM_SIZE);
    shm_unlink(SHM_NAME);

    return 0;
}
