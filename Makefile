all: c rust

c:
	gcc -o mpmt1c mpmt1.c -lpthread
python:
	echo 'Execute `python3 mpmt1.py`'
go:
	go build -o mpmt1go mpmt1.go
rust:
	rustc -o mpmt1rs mpmt1.rs
scala:
	scalac mpmt1.scala
haskell:
	ghc -threaded -rtsopts -o mpmt1hs mpmt1.hs
clean:
	/bin/rm -f *~ *.o mpmt1rs mpmt1c mpmt1go  *.class *.tasty *.dump *.beam mpmt1hs mpmt1.hi mpmt1pas mpmt1cpp *.cmi *.cmo

