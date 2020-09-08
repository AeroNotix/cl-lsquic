#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>

struct sockaddr* gen_sockaddr(int family, const char *ip, u_short port) {
    struct sockaddr_storage *ss = malloc(sizeof(struct sockaddr_storage));

    switch (family)
    {
        case AF_INET:
        {
            struct sockaddr_in *addr = (struct sockaddr_in *) ss;
            addr->sin_family = AF_INET;
            addr->sin_port = htons(port);
            inet_pton(AF_INET, ip, &addr->sin_addr);
            break;
        }

        case AF_INET6:
        {
            struct sockaddr_in6 *addr = (struct sockaddr_in6 *) ss;
            addr->sin6_family = AF_INET6;
            addr->sin6_port = htons(port);
            inet_pton(AF_INET6, ip, &addr->sin6_addr);
            break;
        }

        default:
            return NULL;
    }
    return (struct sockaddr*) ss;
}
