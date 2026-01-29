all: c rust

c:
	gcc -o mpmt1c mpmt1.c -lpthread
python:
	echo 'Execute `python3 mpmt1.py`'
go:
	go build -o mpmt1go mpmt1.go
scala:
	scalac mpmt1.scala
haskell:
	ghc -threaded -rtsopts -o mpmt1hs mpmt1.hs
zigwasm:
	zig build-exe mpmt1.zig -femit-bin=mpmt1zig.wasm -target wasm32-wasi --shared-memory -mcpu=mvp+atomics+bulk_memory --import-memory --export=wasi_thread_start --export-memory  -fno-single-threaded
fortran:
	gfortran mpmt1.f08 -o mpmt1f08 -fopenmp
ada:
	gnatmake mpmt1.adb -o mpmt1adb
vala:
	valac --pkg posix mpmt1.vala -o mpmt1vala
d:
	dmd  -of=mpmt1d  mpmt1.d
wasm: mpmt1-wasi.wasm
	${WASI_SDK_PATH}/bin/clang --sysroot ${WASI_SDK_PATH}/share/wasi-sysroot \
    --target=wasm32-wasi-threads \
    -pthread \
    -Wl,--import-memory,--export-memory,--max-memory=67108864 \
    mpmt1-wasi.c \
    -o mpmt1-wasi.wasm
pli: mpmt1.pli
	plic -C -dELF mpmt1.pli -o mpmt1pli.o
	ld -lpthread -z muldefs -Bstatic -M -o mpmt1pli \
	--oformat=elf32-i386 \
	-melf_i386 \
	-e main \
	 mpmt1pli.o \
	--start-group \
	pli-1.4.0/lib/libprf.a \
	--end-group \
	>mpmt1pli.map

clean:
	/bin/rm -f *~ *.o mpmt1c mpmt1go  *.class *.tasty *.dump *.beam mpmt1hs mpmt1.hi mpmt1pas mpmt1cpp *.cmi *.cmo *.wasm mpmt1f08 mpmt1adb mpmt1.ali mpmt1vala mpmt1pli mpmt1.lst mpmt1pli.map
	cd rust && cargo clean

