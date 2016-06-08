all:
	stack build --no-executable-profiling --no-library-profiling

clean:
	stack clean

prof:
	stack clean
	stack build --executable-profiling --library-profiling --ghc-options -fprof-auto

prun: prof
	stack exec -- graphi 4x4.npz +RTS -p

run:
	stack exec -- graphi 4x4.npz

.PHONY: all prof run
