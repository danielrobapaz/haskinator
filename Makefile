all: oraculo.hs haskinator.hs
	ghc -o haskinator haskinator.hs oraculo.hs

clean: 
	$(RM) oraculo.o oraculo.hi haskinator.o haskinator.hi haskinator