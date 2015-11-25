all:
	stack build

prof:
	stack clean
	stack build --executable-profiling --library-profiling --ghc-options -fprof-auto
	./graphi 4x4.npz +RTS -p
