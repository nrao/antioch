all:simulate serve genhist 

simulate: FORCE
	#ghc -o simulate --make src/Simulate.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16 -prof -auto-all
	ghc -o simulate --make src/Simulate.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16

serve: FORCE
	ghc -o serve --make src/Server/Main.lhs -isrc -isrc/Antioch -O2 -funbox-strict-fields -funfolding-use-threshold=16

genhist: FORCE
	ghc -o genhist --make src/GenHistory.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16

FORCE:

run:
	./simulate

profile:
	./simulate +RTS -p

heap:
	./simulate +RTS -hc

clean:
	find . -name "*~"   -exec $(RM) {} \;
	find . -name "*.hi" -exec $(RM) {} \;
	find . -name "*.o"  -exec $(RM) {} \;

clobber: clean
	$(RM) serve
	$(RM) simulate
	$(RM) genhist
