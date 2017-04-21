// Anna Biadasiewicz 281760

#ifndef SENDING_INCLUDED
#define SENDING_INCLUDED

#include "icmp_aux.h"

int ext_sendto(int sockfd, struct icmphdr * icmp_header, struct sockaddr_in * recipient);
int sending(int pid, int sequence, char* ip_address, int sockfd, int ttl);

#endif
