

Lorenz
======

This is an OpenGL Lorenz Attractor viewer written entirely in Haskell.  It was
written as a homework for a computer graphics class.




Building
--------

This program can be built with the given `Makefile` or with Cabal.


### Makefile

This method will automatically create a Cabal sandbox and install all needed
Haskell dependancies.  You will first need Cabal and an implimentation of GLUT.
The default make rule will build a statically linked `lorenz` binary in the top
level directory.  The optional make rules are listed below.

Build program statically:
```
make static
```

Build program dynamically:
```
make dynamic
```

Clean up temporary build files:
```
make clean-tmp
```

Clean up build files and exicutable (does not remove sandbox):
```
make clean
```

Clean up everything, including the sandbox:
```
make clean-all
```

If you need more control use Cabal.


### Cabal (TODO)





Usage (TODO)
------------

