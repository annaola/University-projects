// Anna Biadasiewicz 281760

#ifndef CHECKSUM_INCLUDED
#define CHECKSUM_INCLUDED

#include <time.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <string.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <sys/select.h>

#define NUM_OF_PACKETS 3
#define MAX_TTL_VALUE 30
#define WAITING_TIME 1

u_int16_t compute_icmp_checksum (const void *buff, int length);
void printRoutersIP(char array[NUM_OF_PACKETS][20]);
int averageTime(long answtimes[NUM_OF_PACKETS]);
int getAnswer(long answtimes[NUM_OF_PACKETS]);
void printData(long answtimes[NUM_OF_PACKETS], clock_t times[NUM_OF_PACKETS], char ips[NUM_OF_PACKETS][20]);

#endif