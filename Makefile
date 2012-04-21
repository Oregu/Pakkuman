CC=ghc
SOURCES=Pacman.hs Pakkuman.hs Keys.hs
EXECUTABLE=pakkuman

all: pacman

pacman:
	$(CC) $(SOURCES) -o $(EXECUTABLE)

clean:
	rm -rf *.o *.hi $(EXECUTABLE)
