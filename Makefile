#
# Makefile
# shlomif, 2018-05-01 20:44
#

all: solver.hi

solver.hi: solver.hs
	ghc -O2 -c -XFlexibleContexts solver.hs

run:
	ghci -O2 -XFlexibleContexts solver.hs

clean:
	rm -f solver.hi solver.o
