all: oraculo.hs haskinator.hs
	ghc -o haskinator -O2 haskinator.hs oraculo.hs
	$(MAKE) clean-objects
clean: 
	$(RM) oraculo.o oraculo.hi haskinator.o haskinator.hi haskinator

clean-objects:
	$(RM) oraculo.o oraculo.hi haskinator.o haskinator.hi