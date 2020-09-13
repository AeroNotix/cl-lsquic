BORINGSSL_COMMIT=251b5169fd44345f455438312ec4e18ae07fd58c

.PHONY: lsquic build test

all: ffi

clone:
	git clone --recursive https://github.com/AeroNotix/lsquic.git
	git clone https://boringssl.googlesource.com/boringssl
	cd boringssl && git checkout ${BORINGSSL_COMMIT}

lsquic:
	cd lsquic && cmake -DAS_SHARED_LIB=1 -DBORINGSSL_DIR=../boringssl . && make -j $(shell nproc)

ffi: src/ffi.lisp src/ffi-dns.lisp src/ffi-udp.lisp

src/ffi.lisp: lsquic.i Makefile
	swig -cffi -noswig-lisp -module ffi -outdir src -I./lsquic/include/ lsquic.i

src/ffi-dns.lisp: dns.i Makefile c-src/libdns.so
	swig -cffi -noswig-lisp -module ffi-dns -outdir src -I./c-src dns.i

src/ffi-udp.lisp: udp.i Makefile c-src/libudp.so
	swig -cffi -noswig-lisp -module ffi-udp -outdir src -I./c-src udp.i
