// Anna Biadasiewicz 281760

#include "icmp_receive.h"

// rozszerzenie funkcji select o wypisywanie ewentualnych błędów
int ext_select(int sockfd, fd_set * descriptors, struct timeval * tv) {
	int ready = select(sockfd+1, descriptors, NULL, NULL, tv);
	if (ready < 0) {
		fprintf(stderr, "select error: %s\n", strerror(errno)); 
		return EXIT_FAILURE;
	}
	if (ready == 0) {
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

// rozszerzenie funkcji recv_from o wypisywanie ewentualnych błędów
int ext_recvfrom(int sockfd, u_int8_t* buffer, struct sockaddr_in * sender, socklen_t * sender_len) {
	ssize_t packet_len = recvfrom (
		sockfd, 
		buffer, 
		IP_MAXPACKET, 
		MSG_DONTWAIT, 
		(struct sockaddr*)sender, 
		sender_len );

	if (packet_len < 0) {
		fprintf(stderr, "recvfrom error: %s\n", strerror(errno)); 
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}

// liczenie czasu oczekiwania na daną odpowiedź
void waitingTimeComputing(struct icmphdr * icmp_header, char ips[NUM_OF_PACKETS][20], long * answtimes, clock_t timesout[NUM_OF_PACKETS], int rcvtime, char * ip_str) {
	int num = (icmp_header->un.echo.sequence)%NUM_OF_PACKETS;
	answtimes[num] = 1000 - rcvtime;
	strcpy(ips[num], ip_str);
}

int receiving(int sockfd, int pid, int * ttl, char ips[NUM_OF_PACKETS][20], long * answtimes, clock_t timesout[NUM_OF_PACKETS]) {
	char ip_str[20];
	int recivedPackets = 0;

	int rcvtime;

	struct timeval tv;
    tv.tv_sec = WAITING_TIME;
    tv.tv_usec = 0;

	fd_set descriptors;
    FD_ZERO (&descriptors);
    FD_SET (sockfd, &descriptors);
    int echo_reply = 0;

	while (recivedPackets != NUM_OF_PACKETS) {
		struct sockaddr_in sender;
		socklen_t sender_len = sizeof(sender);
		u_int8_t buffer[IP_MAXPACKET];
		u_int8_t * ptr = buffer;

		if (ext_select(sockfd, &descriptors, &tv)) break;
		ext_recvfrom(sockfd, buffer, &sender, &sender_len);
		rcvtime = tv.tv_sec * 1000 + tv.tv_usec/1000; // czas pozostały z jednej sekundy od wysłania zapytania

		inet_ntop(AF_INET, &(sender.sin_addr), ip_str, sizeof(ip_str));

		struct iphdr* ip_header = (struct iphdr*) buffer;
		u_int8_t* icmp_packet = buffer + 4 * (ip_header->ihl);
		struct icmphdr* icmp_header = (struct icmphdr*) icmp_packet;

		ptr += 4*ip_header->ihl+8;
		struct iphdr* send_ip_header = (struct iphdr*) ptr;
		ptr += (send_ip_header->ihl * 4);
		struct icmphdr* send_icmp_header = (struct icmphdr*) ptr;

		if (icmp_header->type == ICMP_ECHOREPLY && icmp_header->un.echo.id == pid && (icmp_header->un.echo.sequence)/NUM_OF_PACKETS == *ttl) {
			waitingTimeComputing(icmp_header, ips, answtimes, timesout, rcvtime, ip_str);
			recivedPackets++;
			echo_reply++;
		}

		else if (icmp_header->type == ICMP_TIME_EXCEEDED && send_icmp_header->un.echo.id == pid && (send_icmp_header->un.echo.sequence)/NUM_OF_PACKETS == *ttl) {
			waitingTimeComputing(send_icmp_header, ips, answtimes, timesout, rcvtime, ip_str);
			recivedPackets++;
		}

		else continue;
	}
	return echo_reply;
}