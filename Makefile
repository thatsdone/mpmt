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
clean:
	/bin/rm -f *~ *.o mpmt1c mpmt1go  *.class *.tasty *.dump *.beam mpmt1hs mpmt1.hi mpmt1pas mpmt1cpp *.cmi *.cmo *.wasm mpmt1f08 mpmt1adb mpmt1.ali mpmt1vala
	cd rust && cargo clean

