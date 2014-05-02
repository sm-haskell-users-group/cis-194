# cis194

Homework solutions for UPenn's CIS 194:

http://www.seas.upenn.edu/~cis194/lectures.html

## Installation

On a mac:

    clone repo into cis194
    brew install haskell-platform
    cd cis194
    
Add to .bashrc:

export PATH=$HOME/Library/Haskell/bin:$PATH

Then...

    cabal update
    cabal install hspec
    cabal configure --enable-test
    cabal build

## How to run a single week's tests

```
make week1
```

## How to run ALL the tests

```
cabal configure --enable-tests && cabal build && cabal test
```
