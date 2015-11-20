all:
	stack build
	ln -sf .stack-work/install/x86_64-linux/lts-3.14/7.10.2/bin/graphi

re: clean all

clean:
	stack clean
	rm graphi

run: all
	stack exec graphi ../file.graph
