all:
	stack clean
	stack build
	stack exec graphi file.graph

clean:
	stack clean

run:
	stack exec graphi file.graph

.SILENT:
