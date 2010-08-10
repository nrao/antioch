all: simulate serve 

simulate: src/Antioch/*.lhs
	#ghc -o simulate --make src/Main.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16 -prof -auto-all
	ghc -o simulate --make src/Main.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16

serve:
	ghc -o serve --make src/Server/Main.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16

force:
	touch src/Main.lhs
	touch src/Server/Main.lhs
	make

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
