#include "icmp_sending.h"
#include "icmp_receive.h"
#include "icmp_aux.h"

clock_t timesout[NUM_OF_PACKETS];
long answtimes[NUM_OF_PACKETS];
//clock_t times[NUM_OF_PACKETS];
char ips[NUM_OF_PACKETS][20];

int main(int argc, char const *argv[])
{
    char ip[20];
    
    // sprawdzanie poprawności wejścia
    if (argc != 2) {
        printf("Wrong number of arguments\n");
        return EXIT_FAILURE;
    }

    struct sockaddr_in ip_ad;
    if (inet_pton(AF_INET, ip, &(ip_ad.sin_addr)) == 0) {
        printf("Incorrect ip\n");
        return EXIT_FAILURE;
    }
    
    strcpy(ip, argv[1]);
    
    // tworzenie gniazda
    int sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sockfd < 0) {
        fprintf(stderr, "socket error: %s\n", strerror(errno)); 
        return EXIT_FAILURE;
    }

    int pid = (int)getpid();
    int end = 0;

    for (int ttl = 1; ttl <= MAX_TTL_VALUE; ttl++) {

        // 1. wysyłanie pakietów
        for (int i = 0; i < NUM_OF_PACKETS; i++) {
            sending(pid, 3*ttl+i, ip, sockfd, ttl);
            answtimes[i] = -1;
            memset(ips[i], 0, sizeof(ips[i]));
        }

        // 2. Odbieranie pakietów
        if (receiving(sockfd, pid, &ttl, ips, answtimes, timesout) > 0) end = 1;
        
        // 3. Wypisywanie danych
        printf("%i. ", ttl);
        printData(answtimes, timesout, ips);
        if (end == 1) break;
    }
    return 0;
}
