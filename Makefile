all: Oraculo.hs Haskinator.hs
	ghc -o Haskinator -O2 Haskinator.hs Oraculo.hs Main.hs
	$(MAKE) clean-objects
clean: 
	$(RM) Oraculo.o Oraculo.hi Haskinator.o Haskinator.hi Haskinator Main.hi Main.o

clean-objects:
	$(RM) Oraculo.o Oraculo.hi Haskinator.o Haskinator.hi Main.hi Main.o