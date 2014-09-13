.PHONY: clean

NAME = ass3

all: bake

bake: ass3.hs
	ghc -o ass3 ass3.hs -threaded -XDataKinds

run: bake
	xterm ./ass3
clean:
	rm ass3 ass3.o ass3.hi -f
