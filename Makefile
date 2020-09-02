BORINGSSL_COMMIT=251b5169fd44345f455438312ec4e18ae07fd58c

.PHONY: lsquic build test

lsquic:
	git clone --recursive https://github.com/litespeedtech/lsquic.git --depth 1
	git clone https://boringssl.googlesource.com/boringssl
	cd boringssl && git checkout ${BORINGSSL_COMMIT} && cmake -DCMAKE_BUILD_TYPE=Release . && make -j $(shell nproc)
	cd lsquic && sed -i '/Werror/d' CMakeLists.txt && \
	sed -i 's#<lsquic_types.h>#"lsquic_types.h"#g' include/*h && \
	cmake -DBORINGSSL_DIR=../boringssl . && \
	make -j $(shell nproc)
