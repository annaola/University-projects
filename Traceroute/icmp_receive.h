// Anna Biadasiewicz 281760

#ifndef RECEIVE_INCLUDED
#define RECEIVE_INCLUDED

#include "icmp_aux.h"

int ext_select(int sockfd, fd_set * descriptors, struct timeval * tv);
int ext_recvfrom(int sockfd, u_int8_t* buffer, struct sockaddr_in * sender, socklen_t * sender_len);
void waitingTimeComputing(struct icmphdr * icmp_header, char ips[NUM_OF_PACKETS][20], long * answtimes, clock_t timesout[NUM_OF_PACKETS], int rcvtime, char * ip_str);
int receiving(int sockfd, int pid, int * ttl, char ips[NUM_OF_PACKETS][20], long * answtimes, clock_t timesout[NUM_OF_PACKETS]);

#endif
