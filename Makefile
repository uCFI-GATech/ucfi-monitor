#LLVM_SRC := /home/chenxiong/workspaces/linux-kernel-inconsistency/llvmlinux/toolchain/clang/head/build/llvm
#LLVM_BUILD := /home/chengxiong/llvm-3.6.0/orig-build/
LLVM_BUILD := ~/tools/llvm-3.6.0-install/
KB3_DIR := ${CURDIR}/src
KB3_BUILD := ${CURDIR}/build

NPROC := ${shell nproc}

build_kb3_func = \
	(mkdir -p ${2} \
		&& cd ${2} \
		&& PATH=${LLVM_BUILD}/bin:${PATH} \
			LLVM_ROOT_DIR=${LLVM_BUILD}/bin \
			LLVM_LIBRARY_DIRS=${LLVM_BUILD}/lib \
			LLVM_INCLUDE_DIRS=${LLVM_BUILD}/include \
			CC=gcc CXX=g++ \
            cmake ${1} -DCMAKE_BUILD_TYPE=Release \
                -DCMAKE_CXX_FLAGS_RELEASE="-std=c++11 -fno-rtti -Ofast -fomit-frame-pointer -UNDEBUG_ENABLE -flto -free" \
		&& make -j${NPROC})

build_kb3_func_debug = \
	(mkdir -p ${2} \
		&& cd ${2} \
		&& PATH=${LLVM_BUILD}/bin:${PATH} \
			LLVM_ROOT_DIR=${LLVM_BUILD}/bin \
			LLVM_LIBRARY_DIRS=${LLVM_BUILD}/lib \
			LLVM_INCLUDE_DIRS=${LLVM_BUILD}/include \
			CC=gcc CXX=g++ \
            cmake ${1} -DCMAKE_BUILD_TYPE=Release \
                -DCMAKE_CXX_FLAGS_RELEASE="-g -std=c++11 -fno-rtti -fpic -O0 -DDEBUG_ENABLE" \
		&& make -j${NPROC})

build_kb3_func_run = \
	(mkdir -p ${2} \
		&& cd ${2} \
		&& PATH=${LLVM_BUILD}/bin:${PATH} \
			LLVM_ROOT_DIR=${LLVM_BUILD}/bin \
			LLVM_LIBRARY_DIRS=${LLVM_BUILD}/lib \
			LLVM_INCLUDE_DIRS=${LLVM_BUILD}/include \
			CC=gcc CXX=g++ \
            cmake ${1} -DCMAKE_BUILD_TYPE=Release \
                -DCMAKE_CXX_FLAGS_RELEASE="-g -std=c++11 -fno-rtti -fpic -O3 -UNDEBUG_ENABLE -DDEBUG_RUN" \
		&& make -j${NPROC})

all: kb3

#test: htleak
	#(cd lit-test && make)

kb3:
	$(call build_kb3_func, ${KB3_DIR}, ${KB3_BUILD})

debug:
	$(call build_kb3_func_debug, ${KB3_DIR}, ${KB3_BUILD})

run:
	$(call build_kb3_func_run, ${KB3_DIR}, ${KB3_BUILD})
