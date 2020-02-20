all:
	ghc --make -o bkg-2-cnf bkg-2-cnf.hs
clean:
	rm *.hi *.o bkg-2-cnf

