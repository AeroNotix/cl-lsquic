#include <stdlib.h>
#include <stdio.h>
#include "../lsquic/include/lsxpack_header.h"
#include "../lsquic/include/lsquic_types.h"
#include "../lsquic/include/lsquic.h"
#include <stdlib.h>


void view_lsxpack_header(struct lsxpack_header *hdr) {
    printf("hdr: %p\n", hdr);
    printf("BufP: %p\n", hdr->buf);
    printf("BufS: %s\n", hdr->buf);
    printf("NameLen: %d\n", hdr->name_len);
    printf("NameOff: %d\n", hdr->name_offset);
    printf("ValLen: %d\n", hdr->val_len);
    printf("ValOff: %d\n", hdr->val_offset);
}

void view_header(struct lsquic_http_headers *hdr) {
    printf("Header count: %d | %ld\n", hdr->count, sizeof(struct lsxpack_header));
    for (int x = 0; x < hdr->count; x++) {
        view_lsxpack_header(&(hdr->headers[x]));
    }
}
