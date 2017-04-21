CC = gcc
FLAGS = -Wall -W -Wextra -std=c99

traceroute: icmp_receive.o icmp_sending.o icmp_aux.o traceroute.c
	$(CC) $(FLAGS) -o traceroute icmp_receive.o icmp_sending.o icmp_aux.o traceroute.c

icmp_aux: icmp_aux.h icmp_aux.c
	$(CC) $(FLAGS) -c -o icmp_aux.o icmp_aux.c

icmp_receive: icmp_receive.c icmp_receive.h icmp_aux.o
	$(CC) $(FLAGS) -c -o icmp_receive.o icmp_receive.c icmp_aux.o

icmp_sending: icmp_sending.c icmp_sending.h icmp_aux.o
	$(CC) $(FLAGS) -c -o icmp_sending.o icmp_sending.c icmp_aux.o

clean:
	rm -f *.o
	
distclean:
	rm -f traceroute 