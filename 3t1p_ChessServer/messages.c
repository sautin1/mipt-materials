#include "messages.h"

void throwError(char* error)
{
	perror(error);
	exit(EXIT_FAILURE);
}

MessageType composeMessage(enum Type type, size_t size, void* data)
{
	MessageType m;
	m.type = type;
	m.size = size;
	m.data = malloc(m.size);
	memcpy(m.data, data, m.size);
	return m;
}

int sendMessage(int sd, MessageType* m)
{
	int call_result = 0;
	call_result = send(sd, m, sizeof(MessageType), 0);
	if (call_result == -1){
		throwError("send");
	}
	else {
		//отправляем все побайтово, если известен точный размер
		if (m->type != userlist){
			call_result = send(sd, m->data, m->size, 0);
			if (call_result == -1){
				throwError("send");
			}
		}
		else {
			if (m->size > 0){
				for (int i = 0; i < m->size; ++i){
					call_result = send(sd, ((UserData**)(m->data))[i], sizeof(UserData), 0);
					if (call_result == -1){
						throwError("send");
					}
				}
			}
		}
	}
	return 0;
}

int getMessage(int sd, MessageType* m)
{
	//сначала получаем поля
	int call_result = 0;
	while ((call_result = recv(sd, m, sizeof(MessageType), 0)) == 0);
	if (call_result == -1){
		throwError("recv");
	}
	//теперь выделяем место под data
    if (m->type != userlist){
        if (m->type != logout){
            m->data = malloc(m->size);
            while ((call_result = recv(sd, m->data, m->size, 0)) == 0);
            if (call_result == -1){
                throwError("recv");
            }
        }
	}
	else {
		if (m->size > 0){
			UserData** opponents = (UserData**)malloc(sizeof(UserData*) * m->size);
			for (int i = 0; i < m->size; ++i){
				opponents[i] = (UserData*)malloc(sizeof(UserData));
				while ((call_result = recv(sd, opponents[i], sizeof(UserData), 0)) == 0);
				if (call_result == -1){
					throwError("recv");
				}
			}
			m->data = opponents;
		}
		else {
			m->data = NULL;
		}
	}
	return 0;
}
