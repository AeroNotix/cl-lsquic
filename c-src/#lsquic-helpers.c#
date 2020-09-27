#include <stdlib.h>
#include <stdio.h>
#include "../lsquic/include/lsxpack_header.h"
#include "../lsquic/include/lsquic_types.h"
#include "../lsquic/include/lsquic.h"
#include <stdlib.h>

void view_header(struct lsquic_http_headers *hdr) {
    unsigned char *p = (unsigned char *)hdr;
    printf("Header count: %d\n", hdr->count);
    for (int x = 0; x < hdr->count; x++) {
        printf("BufP: %p\n", hdr->headers[x].buf);
        printf("BufS: %s\n", hdr->headers[x].buf);
        printf("NameLen: %d\n", hdr->headers[x].name_len);
        printf("NameOff: %d\n", hdr->headers[x].name_offset);
        printf("ValLen: %d\n", hdr->headers[x].val_len);
        printf("ValOff: %d\n", hdr->headers[x].val_offset);
        for (int i = 0; i < sizeof(struct lsxpack_header); i++) {
            printf("0x%02x ", p[i]);
        }
        printf("\n");
    }
}

void view_lsxpack_header(struct lsxpack_header *hdr) {
    unsigned char *p = (unsigned char*) hdr;
    printf("New lsxpack header:\n");
    printf("Buf: %p\n", hdr->buf);
    printf("Buf: %s\n", hdr->buf);
    printf("Buf: %d\n", hdr->name_len);
    printf("Buf: %d\n", hdr->name_offset);
    printf("Buf: %d\n", hdr->val_len);
    printf("Buf: %d\n", hdr->val_offset);
    printf("Buf: %d\n", hdr->buf == hdr);
    for (int i = 0; i < sizeof(struct lsxpack_header); i++)
        printf("0x%02x ", p[i]);
    printf("\n");

}
