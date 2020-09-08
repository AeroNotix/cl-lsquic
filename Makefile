BORINGSSL_COMMIT=251b5169fd44345f455438312ec4e18ae07fd58c

.PHONY: lsquic build test

all: src/ffi.lisp src/ffi-dns.lisp

clone:
	git clone --recursive https://github.com/AeroNotix/lsquic.git --depth 1
	cd lsquic && git submodule init && git submodule update
	git clone https://boringssl.googlesource.com/boringssl
	cd boringssl && git checkout ${BORINGSSL_COMMIT}

lsquic:
	cd lsquic && cmake -DAS_SHARED_LIB=1 -DBORINGSSL_DIR=../boringssl . && make -j $(shell nproc)

c-src/libdns.so: c-src/dns.c c-src/dns.h
	cd c-src && make

src/ffi.lisp: lsquic.i Makefile
	swig -cffi -noswig-lisp -module ffi -outdir src -I./lsquic/include/ lsquic.i

src/ffi-dns.lisp: dns.i Makefile
	swig -cffi -noswig-lisp -module ffi-dns -outdir src -I./c-src dns.i
