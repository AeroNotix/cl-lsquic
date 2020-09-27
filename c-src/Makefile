all: dns.so udp.so lsquic-helpers.so

dns.so:
	gcc -c -Wall -fpic dns.c
	gcc -shared -o libdns.so dns.o

udp.so:
	gcc -I../lsquic/include -c -Wall -fpic udp.c
	gcc -shared -o libudp.so udp.o

lsquic-helpers.so:
	gcc -I../lsquic/include -c -Wall -fpic lsquic-helpers.c
	gcc -shared -o liblsquic-helpers.so lsquic-helpers.o
