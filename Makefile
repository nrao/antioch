all:
	ghc -o simulate --make src/Main.lhs -isrc -O2 -funbox-strict-fields -funfolding-use-threshold=16 -prof -auto-all

force:
	touch src/Main.lhs
	make

run:
	./simulate

profile:
	./simulate +RTS -p

heap:
	.simulate +RTS -hc

clean:
	find . -name "*~"   -exec $(RM) {} \;
	find . -name "*.hi" -exec $(RM) {} \;
	find . -name "*.o"  -exec $(RM) {} \;
