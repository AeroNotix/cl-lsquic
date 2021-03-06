BORINGSSL_COMMIT=251b5169fd44345f455438312ec4e18ae07fd58c
LISP ?= sbcl
sbcl_TEST_OPTS=--noinform --disable-debugger --quit --load ./run-tests.lisp

.PHONY: lsquic build test

all: ffi c-libs

image:
	sbcl --load save-executable.lisp

clone:
	git clone --recursive https://github.com/AeroNotix/lsquic.git
	git clone https://boringssl.googlesource.com/boringssl
	cd boringssl && git checkout ${BORINGSSL_COMMIT}

lsquic:
	cd lsquic && cmake -DAS_SHARED_LIB=1 -DBORINGSSL_DIR=../boringssl . && make -j $(shell nproc)

ffi: src/ffi.lisp src/ffi-dns.lisp src/ffi-udp.lisp src/ffi-lsquic-helpers.lisp

src/ffi.lisp: lsquic.i Makefile
	swig -cffi -module ffi -outdir src/lsquic/ -I./lsquic/include/ lsquic.i

src/ffi-dns.lisp: dns.i Makefile c-src/libdns.so
	swig -cffi -module ffi -outdir src/dns/ -I./c-src dns.i

src/ffi-udp.lisp: udp.i Makefile c-src/libudp.so
	swig -cffi -module ffi -outdir src/udp -I./c-src udp.i

src/ffi-lsquic-helpers.lisp: lsquic-helpers.i Makefile c-src/liblsquic-helpers.so
	swig -cffi -module ffi-helpers -outdir src/lsquic -I./c-src lsquic-helpers.i

c-libs:
	cd c-src && make

test:
	@$(LISP) $($(LISP)_TEST_OPTS)
