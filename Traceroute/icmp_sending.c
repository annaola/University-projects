// Anna Biadasiewicz 281760

#include "icmp_sending.h"

int ext_sendto(int sockfd, struct icmphdr * icmp_header, struct sockaddr_in * recipient) {
    ssize_t bytes_sent = sendto(
        sockfd,                         // wybrane gniazdo
        icmp_header,                   // wiadomość
        sizeof(icmp_header),            // długość wiadomości
        0,
        (struct sockaddr*)recipient,   // adres
        sizeof(*recipient) );            // długość adresu

    if (bytes_sent < 0){
        fprintf(stderr, "sendto error: %s\n", strerror(errno)); 
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

int sending(int pid, int sequence, char* ip_address, int sockfd, int ttl) {
    // nagłówek
    struct icmphdr icmp_header;
    icmp_header.type = ICMP_ECHO;
    icmp_header.code = 0;
    icmp_header.un.echo.id = pid;
    icmp_header.un.echo.sequence = sequence;
    icmp_header.checksum = 0;
    icmp_header.checksum = compute_icmp_checksum (
        (u_int16_t*)&icmp_header,
        sizeof(icmp_header) );

    // adresowanie
    struct sockaddr_in recipient;
    bzero(&recipient, sizeof(recipient));
    recipient.sin_family = AF_INET;
    inet_pton(AF_INET, ip_address, &recipient.sin_addr);

    // zmiana ttl
    setsockopt(sockfd, IPPROTO_IP, IP_TTL, &ttl, sizeof(int));

    // wysyłanie pakietu
    ext_sendto(sockfd, &icmp_header, &recipient);

    return EXIT_SUCCESS;
}