all:
	stack clean
	stack build
	stack exec graphi file.graph

re:
	stack clean
	stack build

clean:
	stack clean

run:
	stack exec graphi ../file.graph
