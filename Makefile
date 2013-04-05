HSFILES:=$(shell find . -name "*.hs" | grep -v _darcs)
HIFILES:=$(HSFILES:.hs=.hi)
OFILES:=$(HSFILES:.hs=.o)

default: tester
	./tester

tester: $(HSFILES)
	ghc -Wall -fglasgow-exts -O2  -itests -o $@ --make tests/Main.hs 

prof: $(HSFILES)
	ghc  -prof -auto-all -O2 -itests -o $@ --make tests/Main.hs 

prof.prof: prof
	./prof +RTS -p

slow: $(HSFILES)
	ghc -itests -o $@ --make tests/Main.hs 

debug:
	ghci -fglasgow-exts -itests tests/Main.hs

debugAut:
	ghci -fglasgow-exts -itests Data.Graph.Automorphism

clean: 
	rm -f $(OFILES) $(HIFILES)