// Anna Biadasiewicz 281760

#include "icmp_aux.h"

// funkcja licząca sumę kontrolną (z wykładu)
u_int16_t compute_icmp_checksum (const void *buff, int length)
{
	u_int32_t sum;
	const u_int16_t* ptr = buff;
	assert (length % 2 == 0);
	for (sum = 0; length > 0; length -= 2)
		sum += *ptr++;
	sum = (sum >> 16) + (sum & 0xffff);
	return (u_int16_t)(~(sum + (sum >> 16)));
}

// funkcja wypisująca niepowtarzające się nry ip
void printRoutersIP(char array[NUM_OF_PACKETS][20]) {
    if (array[0] != 0) printf("%s ", array[0]);
    for (int i = 1; i < NUM_OF_PACKETS; i++) {
        int isDiff = 1;
        for (int j = 0; j < i; j++) {
            if (strcmp(array[i], array[j]) == 0) {
                isDiff = 0;
                break;
            }
        }
        if (isDiff == 1) printf("%s ", array[i]);
    }
}

// funkcja licząca średni czas oczekiwania na odpowiedź
int averageTime(long answtimes[NUM_OF_PACKETS]) {
    int long long averageTime = 0;
    for (int i = 0; i < NUM_OF_PACKETS; i++) {
        if (answtimes[i] == -1) {
            return -1;
        }
        else averageTime += answtimes[i];
    }
    return averageTime / 3;
}

// funkcja sprawdzająca, czy nadeszła jakakolwiek odpowiedź
int getAnswer(long answtimes[NUM_OF_PACKETS]) {
    for (int i = 0; i < NUM_OF_PACKETS; i++) {
        if (answtimes[i] != -1) return 1;
    }
    return 0;
}

// funkcja wypisująca informacje o otrzymanych odpowiedziach
void printData(long answtimes[NUM_OF_PACKETS], clock_t timesout[NUM_OF_PACKETS], char ips[NUM_OF_PACKETS][20]) {
    if (getAnswer(answtimes) == 0) printf("*");
    else {
        printRoutersIP(ips);
        if (averageTime(answtimes) != -1) {
            printf("%ims", averageTime(answtimes));
        }
        else printf("???");
    }
    printf("\n");
}
