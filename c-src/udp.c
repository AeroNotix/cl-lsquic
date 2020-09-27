#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <arpa/inet.h>
#include "../lsquic/include/lsquic_types.h"
#include "../lsquic/include/lsquic.h"



#if defined(IP_RECVORIGDSTADDR)
#   define DST_MSG_SZ sizeof(struct sockaddr_in)
#else
#   define DST_MSG_SZ sizeof(struct in_pktinfo)
#endif

#define ECN_SZ CMSG_SPACE(sizeof(int))

/* Amount of space required for incoming ancillary data */
#define CTL_SZ ECN_SZ

int send_packets_out(struct sockaddr* dest_sa, struct iovec *iov, size_t iovlen, int fd) {

    printf("Receiving packets on fd: %d\n", fd);

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

static void
_process_ancillary(struct msghdr *msg, int *ecn) {
    struct cmsghdr *cmsg;

    for (cmsg = CMSG_FIRSTHDR(msg); cmsg; cmsg = CMSG_NXTHDR(msg, cmsg))
    {
        if ((cmsg->cmsg_level == IPPROTO_IP && cmsg->cmsg_type == IP_TOS)
                 || (cmsg->cmsg_level == IPPROTO_IPV6
                                            && cmsg->cmsg_type == IPV6_TCLASS))
        {
            memcpy(ecn, CMSG_DATA(cmsg), sizeof(*ecn));
            *ecn &= IPTOS_ECN_MASK;
        }
    }
}

int
recv_packets_in(struct lsquic_engine *engine, struct sockaddr *local_sas, int fd, void *peer_ctx) {
    ssize_t nread;
    int ecn;
    struct sockaddr_storage peer_sas;
    unsigned char buf[0x600];
    struct iovec vec[1] = {{ buf, sizeof(buf) }};
    unsigned char ctl_buf[CTL_SZ];

    struct msghdr msg = {
        .msg_name       = &peer_sas,
        .msg_namelen    = sizeof(peer_sas),
        .msg_iov        = vec,
        .msg_iovlen     = 1,
        .msg_control    = ctl_buf,
        .msg_controllen = sizeof(ctl_buf),
    };
    nread = recvmsg(fd, &msg, 0);
    if (-1 == nread) {
        if (!(EAGAIN == errno || EWOULDBLOCK == errno))
            perror("recvmsg");
        // return 0 so the Lisp code can think this was successful and
        // reschedule us.
        return 0;
    }

    ecn = 0;
    _process_ancillary(&msg, &ecn);
    return lsquic_engine_packet_in(engine,
                                   buf,
                                   nread,
                                   local_sas,
                                   (struct sockaddr *) &peer_sas,
                                   peer_ctx,
                                   ecn);
}
