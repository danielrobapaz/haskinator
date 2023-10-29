all: oraculo.hs haskinator.hs
	ghc -o haskinator -O2 haskinator.hs oraculo.hs main.hs
	$(MAKE) clean-objects
clean: 
	$(RM) oraculo.o oraculo.hi haskinator.o haskinator.hi haskinator main.hi main.o

clean-objects:
	$(RM) oraculo.o oraculo.hi haskinator.o haskinator.hi main.hi main.o