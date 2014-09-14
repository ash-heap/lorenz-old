# Commands:
#	make			= builds program statically
#	make static		= builds program statically
#	make dynamic	= builds program dynamically
#	make all		= builds everything (builds program statically)
#	make clean		= cleans up the build but does not remove the sandbox
#	make clean-tmp	= same as clean but does not remove the executable
#	make clean-all	= same as clean but also removes the sandbox


### Configuration
################################################################################

# Program Name
PROGRAM=lorenz

# Sources
SOURCES=Main.hs

# Haskell libraries.
LIBS=GLUT

################################################################################


### Internal Variables
################################################################################
static: GHCFLAGS=-O2
dynamic: GHCFLAGS=-O2 -dynamic
################################################################################


### Rules
################################################################################

.PHONY: all
all: static

# Make a static binary.
.PHONY: static
static: libs $(PROGRAM)

# Make a dynamic binary.
.PHONY: dynamic
dynamic: libs $(PROGRAM)

# Install a cabal sandbox.
.PHONY: sandbox
sandbox: .cabal-sandbox

# Install libraries.
.PHONY: libs
libs: sandbox
	cabal --require-sandbox install $(LIBS)

# Clean up the build.
.PHONY: clean 
clean: clean-tmp
	rm -f $(PROGRAM)

# Clean up the intermediate build files.
.PHONY: clean-tmp
clean-tmp:
	cabal clean

# Clean up all files including the binary and sandbox.
.PHONY: clean-all
clean-all: clean clean-sandbox

################################################################################


### Internal Rules
################################################################################

.cabal-sandbox:
	cabal sandbox init

.PHONY: clean-sandbox
clean-sandbox:
	cabal sandbox delete

dist:
	cabal configure --ghc-options="$(GHCFLAGS)"

$(PROGRAM): dist/build/$(PROGRAM)/$(PROGRAM)
	cp dist/build/$(PROGRAM)/$(PROGRAM) $(PROGRAM)

dist/build/$(PROGRAM)/$(PROGRAM): $(SOURCES) dist
	cabal build

################################################################################
