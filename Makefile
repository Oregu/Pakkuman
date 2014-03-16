CC=ghc
SRCDIR := src
OBJDIR := build
EXECUTABLE=pakkuman

all: pacman

pacman:
	$(CC) $(SRCDIR)/*.hs -odir $(OBJDIR) -o $(EXECUTABLE)

clean:
	rm -rf $(OBJDIR) $(EXECUTABLE)
