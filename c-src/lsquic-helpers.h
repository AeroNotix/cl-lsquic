#include "../lsquic/include/lsxpack_header.h"
#include "../lsquic/include/lsquic_types.h"
#include "../lsquic/include/lsquic.h"


struct lsquic_http_headers *new_headers(size_t header_count);
void add_header(const char *name, const char *val);
void view_header(struct lsquic_http_headers *hdr);
void view_lsxpack_header(struct lsxpack_header *hdr);
