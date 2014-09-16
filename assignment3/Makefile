.PHONY: clean

NAME = ass3

all: bake

bake: ass3.hs
	ghc -o ass3 ass3.hs src/LanTokenizer.hs src/LanPrettyprint.hs -XDataKinds

run: bake
	xterm ./ass3 -geometry 150x50
clean:
	rm ass3 *.o *.hi -f
