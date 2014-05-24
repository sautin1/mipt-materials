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
        //send byte-by-byte, if the precise size is known
		if (m->type != userlist){
			call_result = send(sd, m->data, m->size, 0);
			if (call_result == -1){
				throwError("send");
			}
		}
		else {
			if (m->size > 0){
                size_t opponent_q = m->size / sizeof(UserData*);
                for (int i = 0; i < opponent_q; ++i){
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
    //first get fields
	int call_result = 0;
	while ((call_result = recv(sd, m, sizeof(MessageType), 0)) == 0);
	if (call_result == -1){
		throwError("recv");
	}
    //then malloc space for data
    switch (m->type){
        case userlist:
        {
            if (m->size > 0){
                size_t opponent_q = m->size / sizeof(UserData*);
                UserData** opponents = (UserData**)malloc(sizeof(UserData*) * m->size);
                for (int i = 0; i < opponent_q; ++i){
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
            break;
        }

        case logout:
        {
            break;
        }

        default:
        {
            m->data = malloc(m->size);
            while ((call_result = recv(sd, m->data, m->size, 0)) == 0);
            if (call_result == -1){
                throwError("recv");
            }
        }

    }

	return 0;
}
