#
# Makefile
# shlomif, 2018-05-01 20:44
#

all: solver.hi

solver.hi: solver.hs
	ghc -c -XFlexibleContexts solver.hs

run:
	ghci -XFlexibleContexts solver.hs
