#
# Makefile
# shlomif, 2018-05-01 20:44
#

SHELL = /bin/bash
GHC = ghc
HFLAGS = -XFlexibleContexts

all: solver.hi

solver.hi: solver.hs
	$(GHC) $(HFLAGS) -O2 -c solver.hs

solver.exe: solver.hs solver_main.hs
	$(GHC) $(HFLAGS) -O2 -o $@ $^

run:
	ghci -O2 $(HFLAGS) solver.hs

clean:
	rm -f solver.exe solver.hi solver.o

test: all solver.exe
	prove tests/*.{py,t}
