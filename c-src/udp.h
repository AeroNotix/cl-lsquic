#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include "../lsquic/include/lsquic_types.h"
#include "../lsquic/include/lsquic.h"


int send_packets_out(struct sockaddr* dest_sa, struct iovec *iov, size_t iovilen, int fd);
