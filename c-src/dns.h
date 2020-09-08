#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

struct sockaddr* gen_sockaddr(int family, const char *ip, u_short port);
