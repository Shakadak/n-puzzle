graphi:
	stack build
	@ln -sf .stack-work/install/x86_64-linux/lts-3.14/7.10.2/bin/graphi

re: clean graphi

clean:
	stack clean
	rm graphi

run: graphi
	stack exec graphi ../maps/20x20.graph
