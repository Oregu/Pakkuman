CC=ghc
SOURCES=Pacman.hs GameState.hs Pakkuman.hs Draw.hs Keys.hs
EXECUTABLE=pakkuman

all: pacman

pacman:
	$(CC) $(SOURCES) -o $(EXECUTABLE)

clean:
	rm -rf *.o *.hi $(EXECUTABLE)

