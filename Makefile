all: oraculo.hs haskinator.hs
	ghc -o haskinator haskinator.hs oraculo.hs
	$(MAKE) clean

clean: 
	$(RM) oraculo.o oraculo.hi haskinator.o haskinator.hi