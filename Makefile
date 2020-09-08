BORINGSSL_COMMIT=251b5169fd44345f455438312ec4e18ae07fd58c

.PHONY: lsquic build test

all: src/ffi.lisp src/ffi-dns.lisp

lsquic:
	git clone --recursive https://github.com/litespeedtech/lsquic.git --depth 1
	git clone https://boringssl.googlesource.com/boringssl
	cd boringssl && git checkout ${BORINGSSL_COMMIT} && cmake -DCMAKE_BUILD_TYPE=Release . && make -j $(shell nproc)
	cd lsquic && sed -i '/Werror/d' CMakeLists.txt && \
	sed -i 's#<lsquic_types.h>#"lsquic_types.h"#g' include/*h && \
	cmake -DBORINGSSL_DIR=../boringssl . && \
	make -j $(shell nproc)

src/ffi.lisp: lsquic.i Makefile
	swig -cffi -noswig-lisp -module ffi -outdir src -I./lsquic/include/ lsquic.i

src/ffi-dns.lisp: dns.i Makefile
	swig -cffi -noswig-lisp -module ffi-dns -outdir src -I./c-src dns.i
