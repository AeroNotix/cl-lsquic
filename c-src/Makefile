dns.so:
	gcc -c -Wall -fpic dns.c
	gcc -shared -o libdns.so dns.o

udp.so:
	gcc -I../lsquic/include -c -Wall -fpic udp.c
	gcc -shared -o libudp.so udp.o
