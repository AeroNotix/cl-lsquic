#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include "../lsquic/include/lsquic_types.h"
#include "../lsquic/include/lsquic.h"

int send_packets_out(struct sockaddr* dest_sa, struct iovec *iov, size_t iovlen, int fd) {
    struct msghdr msg;
    msg.msg_flags      = 0;
    msg.msg_control    = NULL;
    msg.msg_controllen = 0;
    msg.msg_name       = (void *) dest_sa;
    msg.msg_namelen    = (AF_INET == dest_sa->sa_family ?
                          sizeof(struct sockaddr_in) :
                          sizeof(struct sockaddr_in6));
    msg.msg_iov        = iov;
    msg.msg_iovlen     = iovlen;
    return sendmsg(fd, &msg, 0);
}
